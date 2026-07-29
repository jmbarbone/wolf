#' Wolf Logger
#'
#' @param name Name of your logger
#' @param path Path to store log files
#' @param level Logging level; one of "debug", "info", "warn", "error", or
#' @param ... Additional arguments passed to `Wolf$new()`
#' @param .default Logical; if `TRUE`, adds the default handler.
#' @export
wolf <- function(
  name = basename(getwd()),
  path = getwd(),
  level = c("debug", "info", "warn", "error", "none"),
  ...,
  .default = TRUE
) {
  Wolf$new(name, path, level, ..., .default = .default)
}

#' Wolf
#'
#' @description `wolf` is a simple logging utility for R that captures and logs
#' conditions (messages, warnings, errors, etc.) during code execution. It
#' allows you to define custom handlers for different types of conditions and
#' log them to various formats (text files, CSV, TSV, DCF).
#'
#' @param name Name of your logger
#' @md
#' @examples
#' w <- wolf("example", .default = FALSE)
#' my_handler <- LogHandler$new("e1", path = tempfile(fileext = ".log"))
#' w$add_handler(my_handler)
#' # ensure that logging is written to the stdout()
#' w$add_handler(ConnectionHandler$new("e2", description = ""))
#' w$log(message("this"))
#' w$debug("debugging info")
#' readLines(my_handler$path)
#' invisible(file.remove(my_handler$path))
#' @export
# nolint next: object_name_linter.
Wolf <- R6::R6Class(
  "Wolf",
  active = list(
    #' @field level The logging level.  Automatically validated when set.
    level = local({
      ..level <- character() # nolint: object_name_linter.
      function(value) {
        if (missing(value)) {
          return(..level)
        }

        value <- fuj::match_arg(
          value,
          c("debug", "info", "warn", "error", "none")
        )

        private$levels <- switch(
          value,
          debug = c("debug", "info", "warn", "error"),
          info = c("info", "warn", "error"),
          warn = c("warn", "error"),
          error = "error",
          none = character()
        )

        ..level <<- value # nolint: object_name_linter.
      }
    }),

    #' @field handlers The list of handlers attached to the logger. Read only.
    #'   To modify handlers, use `$add_handler()` or `$remove_handler()`.
    handlers = local({
      function(value) {
        if (!missing(value)) {
          stop(
            "cannot set 'handlers' directly; use 'add_handler()' or",
            " 'remove_handler()'",
            call. = FALSE
          )
        }

        as.list(private$handlers_)
      }
    }),

    #' @field entries The list of log entries captured by the logger. Read only.
    entries = local({
      function(value) {
        if (!missing(value)) {
          stop(
            "cannot set 'entries' directly; this is a read-only field",
            call. = FALSE
          )
        }

        # TODO implement this
        private$entries_
      }
    })
  ),
  public = list(
    #' @field name The name of the wolf logger
    name = NULL,

    #' @field path The path to store log files
    path = NULL,

    #' @description Create a new Wolf logger.
    #' @param name Name of the logger. Defaults to the current working
    #'   directory's basename.
    #' @param path Path to store log files. Defaults to the current working
    #'   directory.
    #' @param level Logging level; one of "debug", "info", "warn", "error", or
    #'   "none".
    #' @param ... Additional arguments (currently unused).
    #' @param .default Logical; if `TRUE`, adds the default handler.
    initialize = function(
      name = basename(getwd()),
      path = getwd(),
      level = c("debug", "info", "warn", "error", "none"),
      ...,
      .default = TRUE
    ) {
      self$name <- name
      self$path <- path
      self$level <- level
      self
    },

    #' @description Evaluate an expression and log any conditions that arise
    #' @param expr An expression to evaluate.
    #' @param frame The parent frame
    log = function(expr, frame = parent.frame()) {
      time <- Sys.time()
      expr <- substitute(expr)
      withRestarts(
        invokeRestart(
          "implement_wolf_handlers",
          tryCatch(
            eval.parent(expr),
            condition = function(cnd) {
              cnd$call <- NULL
              list(
                condition = cnd,
                frame = frame,
                time = time,
                expr = expr
              )
            }
          )
        ),
        implement_wolf_handlers = private$impl_handlers
      )
    },

    #' @name add_handler
    #' @description Add a handler to the logger.
    #' @param ... a named list of handlers to add. Any character strings are
    #'   assumed to be built-in default handlers
    add_handler = function(...) {
      handlers <- list(...)
      names <- names(handlers) %||% rep("", length(handlers))

      handlers <- mapply(
        function(name, handler) {
          if (inherits(handler, "WolfHandler")) {
            if (nzchar(name)) {
              handler$name <- name
            }
            return(handler)
          }

          if (!nzchar(name)) {
            stop(fuj::input_error("cannot add unnamed R6 handler"))
          }

          if (inherits(handler, "R6ClassGenerator")) {
            handler$new(name = name)
          } else if (is.character(handler)) {
            switch(
              handler,
              "log" = LogHandler$new(name = name),
              "csv" = TableHandler$new(name = name, type = "csv"),
              "tsv" = TableHandler$new(name = name, type = "tsv"),
              "dcf" = TableHandler$new(name = name, type = "dcf"),
              stop(fuj::input_error("unknown handler type: ", handler))
            )
          }
        },
        name = names,
        handler = handlers,
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )

      names(handlers) <- fuj::vap_chr(handlers, "name")
      private$append_handlers(handlers)
      invisible(self)
    },

    #' @description Remove a handler by name
    #' @param name Name of the handler to remove.
    remove_handler = function(name) {
      m <- match(name, names(self$handlers))
      if (is.na(name)) {
        stop("no handler with that name")
      }
      self$handlers <- self$handlers[-m]
      invisible(self)
    },

    #' @description Log a debug message.
    #' @param ... Messages to log.
    debug = function(...) {
      private$signal("debug", ...)
      invisible(self)
    },

    #' @description Log an info message.
    #' @param ... Messages to log.
    #'
    info = function(...) {
      private$signal("info", ...)
      invisible(self)
    },

    #' @description Log a warning message.
    #' @param ... Messages to log.
    warn = function(...) {
      private$signal("warn", ...)
      invisible(self)
    },

    #' @description Log an error message.
    #' @param ... Messages to log.
    error = function(...) {
      private$signal("error", ...)
      invisible(self)
    }
  ),
  private = list(
    levels = character(),
    handlers_ = list(),
    entries_ = list(),
    impl_handlers = function(spec) {
      type <- get_type(spec$condition)
      type <- switch(
        type,
        message = "info",
        warning = "warn",
        condition = "debug", #?
        type
      )

      entry <- wolf_entry(
        name = self$name,
        message = conditionMessage(spec$condition),
        time = spec$time,
        type = type,
        frame = spec$frame,
        expr = spec$expr
      )

      private$append_entries(entry)

      for (handler in self$handlers) {
        if (type %in% private$levels) {
          handler$process(entry)
        }
      }
    },

    append_entries = function(entry) {
      private$entries_ <- do.call(
        wolf_entries,
        c(private$entries_, list(entry))
      )
    },

    append_handlers = function(new) {
      # silently removes duplicate handlers
      private$handlers_ <- c(new, private$handlers_)
      dupes <- duplicated(names(private$handlers_), fromLast = TRUE)
      private$handlers_ <- private$handlers_[
        !duplicated(names(private$handlers_), fromLast = TRUE)
      ]
    },

    signal = function(
      type = c("debug", "info", "warn", "error"),
      ...
    ) {
      type <- fuj::match_arg(type)
      if (!type %in% private$levels) {
        return()
      }

      cnd <- list(message = paste0(..., collapse = ""), call = NULL)
      class(cnd) <- c("condition", type)
      op <- options(show.error.messages = FALSE)
      on.exit(options(op), add = TRUE)
      self$log(signalCondition(cnd))
    }
  )
)

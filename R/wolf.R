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
#' temp <- tempfile()
#' write_to_temp <- function(cnd) {
#'   cat(
#'     trimws(conditionMessage(cnd)),
#'     "\n",
#'     sep = "",
#'     file = temp,
#'     append = TRUE
#'   )
#' }
#' w <- wolf("example", .default = FALSE)
#' w$add_handler(temp_writer = write_to_temp)
#' w$log(message("this"))
#' w$debug("debugging info")
#' readLines(temp)
#' invisible(file.remove(temp))
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

        value <- match.arg(value, c("debug", "info", "warn", "error", "none"))

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
    entires = local({
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
      if (.default) {
        self$add_handler("default")
      }
    },

    #' @description Evaluate an expression and log any conditions that arise
    #' @param expr An expression to evaluate.
    log = function(expr) {
      expr <- substitute(expr)
      withRestarts(
        invokeRestart(
          "implement_wolf_handlers",
          tryCatch(eval.parent(expr), condition = \(cnd) {
            cnd$call <- NULL
            cnd
          })
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
      w <- which(vapply(handlers, is.character, NA))
      stopifnot(vapply(handlers[-w], is.function, NA))

      for (i in w) {
        # fmt: skip
        # nolint start: line_length_linter.
        handler <- switch(
          handlers[[i]],
          default = list(.default = wolf_default_handler),
          file = ,
          log = list(.default_file_handler_log = wolf_default_writer_log(self$name, self$path)),
          csv = list(.default_file_handler_csv = wolf_default_writer_csv(self$name, self$path)),
          tsv = list(.default_file_handler_tsv = wolf_default_writer_tsv(self$name, self$path)),
          dcf = list(.default_file_handler_dcf = wolf_default_writer_dcf(self$name, self$path)),
          stdout = list(.default_stdout_handler = wolf_default_con_handler(self$name, stdout())),
          stderr = list(.default_stderr_handler = wolf_default_con_handler(self$name, stderr())),
          null = list(.default_null_handler = \() NULL),
          # nolint end: line_length_linter.
          {
            warning(
              "unknown default handler type: ",
              handlers[[i]],
              call. = FALSE
            )
            NULL
          }
        )

        if (is.null(handler)) {
          handlers[[i]] <- NA
          next
        }

        handlers[[i]] <- NULL
        handlers <- append(handlers, handler, i - 1L)
      }

      handlers <- handlers[!vapply(handlers, identical, NA, NA)]
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
    impl_handlers = function(cnd) {
      time <- Sys.time()
      type <- get_type(cnd)

      spec <- list(
        time = time,
        type = type, # TODO use "level"
        name = name,
        message = conditionMessage(cnd),
        call = conditionCall(cnd),
        classes = class(cnd)
      )
      class(spec) <- "wolf_log_entry"

      # store original type in spec
      type <- switch(
        type,
        message = "info",
        warning = "warn",
        type
      )

      for (handler in self$handlers) {
        if (type %in% private$levels) {
          handler(cnd)
          # TODO use do.call(handler, spec)
        }
      }
    },

    append_handlers = function(new) {
      stopifnot(is.list(new), nzchar(names(new)))
      private$handlers_ <- c(new, private$handlers_)

      # templated from globalCallingHandlers()
      for (name in unique(names(private$handlers_))) {
        w <- which(name == names(private$handlers_))
        handlers <- lapply(private$handlers_[w], unclass)
        dups <- duplicated(handlers)

        if (any(dups)) {
          message(sprintf(
            "pushing duplicate `%s` handler on top of the stack",
            name
          ))
          private$handlers_ <- private$handlers_[-w[dups]]
        }
      }
    },

    signal = function(
      type = c("debug", "info", "warn", "error"),
      ...
    ) {
      type <- match.arg(type)
      if (!type %in% private$levels) {
        return()
      }

      cnd <- structure(
        list(
          message = paste0(..., collapse = ""),
          call = NULL
        ),
        class = c("condition", type)
      )
      op <- options(show.error.messages = FALSE)
      on.exit(options(op), add = TRUE)
      self$log(signalCondition(cnd))
    }
  )
)

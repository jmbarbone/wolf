# write(entry) should then handle everything within the entry
# close() should be used to handle anything

# new_handler <- function(name, write = NULL, close = NULL, ...) {
#   public <- list(write = write, close = close)
#   public <- public[!vapply(public, is.null, NA)]
#   R6::R6Class(name, inherit = WolfHandler, public = public)
# }

# public methods should not be modified

#' WolfHandler
#'
#' Base [R6::R6Class()] for all handlers.
#'
#' @export
WolfHandler <- R6::R6Class(
  "WolfHandler",
  active = list(
    #' @field entries A list of [wolf::wolf_entry()] objects
    entries = local({
      function(value) {
        if (!missing(value)) {
          stop(
            "cannot set 'entries' directly; this is a read-only field",
            call. = FALSE
          )
        }

        private$entries_
      }
    }),

    #' @field queued A list of [wolf::wolf_entry()] objects that have not yet been written
    queued = local({
      function(value) {
        if (!missing(value)) {
          stop(
            "cannot set 'queued' directly; this is a read-only field",
            call. = FALSE
          )
        }
        private$queue_
      }
    })
  ),
  public = list(
    #' @field name Identifier
    name = NULL,

    #' @field active Is active
    active = TRUE,

    #' @field delay Whether entries should be written immediately or not.
    #' For intrusive writing processes, delaying can be more effficient.  This
    #' can be toggled on/off during processing as needed
    delay = FALSE,

    #' @param name name identifier
    #' @param ... Additional arguments (not used)
    #' @description Initialized object
    initialize = function(name, ...) {
      self$name <- name
      private$validate()
      self
    },

    #' @param entry A [wolf::wolf_entry()] object that will be passed into the
    #' class
    #' @description Processes entries; the main driver
    process = function(entry) {
      if (!self$active) {
        # completely ignores entry; doesn't bother to save
        return(self)
      }

      if (self$delay) {
        # simply add to queue -- don't perform any other actions
        private$add_queue(entry)
      } else {
        private$add_entry(entry)
        self$single_writer(entry)
      }

      self
    },

    #' @param entry A [wolf::wolf_entry()] object
    #' @description Calls the private writer method.  When `sel$delay = FALSE`,
    #' and there are entries in `self$queued`, each file is written.  To control
    #' _how_ the entries are written, modify the `$writer()` method
    write = function(entry) {
      if (!private$initialized) {
        private$init_writer()
        private$initialized <- TRUE
      }

      if (!self$delay) {
        private$write_delayed()
      }

      self$single_writer(entry)
    },

    #' @param entry A [wolf::wolf_entry()] object
    #' @description Writes a single `entry` object.  This is the default writer
    #' method and can be modified to change how entries are written.
    single_writer = function(entry) {
      NULL
    },

    #' @param entries A list of [wolf::wolf_entry()] objects
    #' @description A special writer method to handle writing multiple entries
    #' at once.
    bulk_writer = function(entries) {
      for (entry in entries) {
        self$single_writer(entry)
      }
    },

    #' @description A one-time setup method for the writer.  This may be
    #' necessary for certain handlers, like [wolf::TableHandler] which sets up
    #' the column names in a file before writing
    setup_writer = function() {
      NULL
    },

    #' @description Read entries
    read = function() {
      self$entries
    },

    #' @description Calls the private `$finalize()` method
    close = function() {
      self$delay <- FALSE
      private$write_delayed()
      private$finalize()
    }
  ),
  private = list(
    initialized = FALSE,
    entries_ = NULL,
    queue_ = NULL,

    add_entry = function(entry) {
      private$entries_ <- c(private$entries_, list(entry))
    },

    add_queue = function(entry) {
      private$queue_ <- c(private$queue_, list(entry))
    },

    write_delayed = function() {
      if (!is.null(private$queue_)) {
        private$bulk_writer(private$queue_)
        private$queue_ <- NULL
      }
    },
    # modifiable
    validate = function() {
      NULL
    },
    finalize = function() {
      if (self$active) {
        private$write_delayed()
      }
    }
  )
)


#' Wolf default handlers
#'
#' @param name Name of the handler.
#' @param path Path to use for file-based handlers.
#' @param con Connection to use for connection-based handlers.
#' @param cnd Condition to handle.
#' @name wolf_handlers
NULL

wolf_handler <- function(
  name,
  handler,
  path = NULL,
  reader = NULL,
  formatter = NULL
) {
  structure(
    list(
      name = name,
      handler = handler,
      path = path,
      reader = reader,
      formatter = formatter
    ),
    class = c("wolf_handler", "list")
  )
}

c("keepNA", "keepInteger", "niceNames")

#' @export
print.wolf_handler <- function(x, ...) {
  fun <- utils::capture.output(utils::str(x$handler))
  fun <- paste0(fun, collapse = " ")
  cat("<wolf handler>\n")
  cat("  name:     ", x$name, "\n", sep = "")
  cat("  handler:  ", fun, "\n", sep = "")
  cat("  path:     ", x$path %||% "<>", "\n", sep = "")
  cat("  reader:   ", x$reader %||% "<>", "\n", sep = "")
  cat("  formatter:", x$formatter %||% "<>", "\n", sep = "")
}

wolf_handler_spec <- list(
  handler = "some_function",
  path = NULL,
  reader = "some_function",
  formatter = "some_function"
)

#' @rdname wolf_handlers
#' @export
wolf_default_handler <- function(cnd) {
  switch(
    get_type(cnd),
    message = message(cnd),
    warning = warning(cnd),
    error = stop(cnd),
    cat(conditionMessage(cnd), "\n", sep = "")
  )
}

#' @rdname wolf_handlers
#' @export
wolf_default_con_handler <- function(name, con) {
  function(cnd) {
    msg <- sprintf(
      "%s [%s] (%s) %s\n",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      get_type(cnd),
      name,
      paste0(conditionMessage(cnd), collapse = "")
    )

    msg <- paste(msg, collapse = "")
    msg <- trimws(msg)
    msg <- paste0(msg, "\n")
    cat(msg, file = con)
  }
}

#' @rdname wolf_handlers
#' @export
wolf_default_writer_log <- function(name, path) {
  .name <- name
  .path <- default_writer_setup(name, path, "log")
  function(cnd) {
    f <- file(.path, "a+b", encoding = "UTF-8")
    on.exit(close(f), add = TRUE)

    msg <- sprintf(
      "%s [%s] (%s) %s\n",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      get_type(cnd),
      .name,
      paste0(conditionMessage(cnd), collapse = "")
    )

    msg <- paste(msg, collapse = "")
    msg <- trimws(msg)
    msg <- paste0(msg, "\n")
    msg <- charToRaw(msg)
    writeBin(msg, f)
  }
}

#' @rdname wolf_handlers
#' @export
wolf_default_writer_csv <- function(name, path) {
  .name <- name
  .path <- default_writer_setup(name, path, "csv.log")
  function(cnd) {
    exists <- file.exists(.path)
    f <- file(.path, "ab", encoding = "UTF-8")
    on.exit(close(f), add = TRUE)
    utils::write.table(
      fuj::quick_df(list(
        time = Sys.time(),
        type = get_type(cnd),
        name = .name,
        message = trimws(conditionMessage(cnd))
      )),
      file = f,
      sep = ",",
      dec = ".",
      qmethod = "double",
      row.names = FALSE,
      append = exists,
      col.names = !exists
    )
  }
}

#' @rdname wolf_handlers
#' @export
wolf_default_writer_tsv <- function(name, path) {
  .name <- name
  .path <- default_writer_setup(name, path, "tsv.log")
  function(cnd) {
    exists <- file.exists(.path)
    f <- file(.path, "ab", encoding = "UTF-8")
    on.exit(close(f), add = TRUE)
    utils::write.table(
      fuj::quick_df(list(
        time = Sys.time(),
        type = get_type(cnd),
        name = .name,
        message = trimws(conditionMessage(cnd))
      )),
      file = f,
      sep = "\t",
      append = exists,
      col.names = !exists,
      row.names = FALSE,
    )
  }
}

#' @rdname wolf_handlers
#' @export
wolf_default_writer_dcf <- function(name, path) {
  .path <- default_writer_setup(name, path, "dcf.log")
  .name <- name
  function(cnd) {
    f <- file(.path, "ab", encoding = "UTF-8")
    on.exit(close(f), add = TRUE)
    write.dcf(
      list(
        time = Sys.time(),
        type = get_type(cnd),
        name = .name,
        message = trimws(conditionMessage(cnd))
      ),
      file = f,
      append = TRUE,
      indent = 2L,
      width = Inf,
    )
    cat("\n", file = f)
  }
}

default_writer_setup <- function(name, path, ext = "log") {
  dir.create(path, FALSE, TRUE)
  name <- sprintf("wolf-%s.%s", name, ext)
  print(normalizePath(file.path(path, name), "/", FALSE))
}

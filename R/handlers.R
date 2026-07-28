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
#' Public methods need to be called directly through the [wolf::Wolf] class
#' 
#' Public fields can include different settings
#' 
#' @export
WolfHandler <- R6::R6Class(
  "WolfHandler",
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
        self$write(entry)
      }

      self
    },

    #' @param entry A [wolf::wolf_entry()] object
    #' @description Calls the private writer method.  When `sel$delay = FALSE`,
    #' and there are entries in `self$queued`, each file is written 
    write = function(entry) {
      if (!private$initialized) {
        private$init()
        private$initialized <- TRUE
      }

      if (!self$delay) {
        private$write_delayed()
      }

      private$writer(entry)
    },

    #' @description Read entries from the handler
    read = function() {
      private$reader()
    },
    
    #' @description Calls the private finalize() method
    close = function() {
      self$delay <- FALSE
      private$finalize()
    }
  ),
  private = list(
    initialized = FALSE,
    entries = wolf_entry(),
    queue = wolf_entries(),
    add_entry = function(entry) {
      private$entries <- do.call(wolf_entries,  c(private$entries, list(entry)))
    },
    add_queue = function(entry) {
      private$queue <- do.call(wolf_entries,  c(private$queue, list(entry)))
    },
    write_delayed = function() {
      if (is.function(private$queued_writer)) {
        private$queued_writer(private$queue)
        private$queue <- wolf_entries()
      } else {
        for (q in private$queued) {
          private$writer(q)
          private$queue <- private$queue[-1L]
        }
      }
    },

    # modifiable
    finalize = function() {
      if (self$active) {
        private$write_delayed()
      }
    },
    writer = function(entry) NULL,
    queued_writer = NULL,
    reader = function() private$entries,
    init = function() NULL
  )
)

#' Connection handler
#' 
#' Write entries to a connection
#' 
#' No default `private$writer()` methods applied
#' 
#' @export
ConnectionHandler <- R6::R6Class(
  "WolfConnectionHandler",
  inherit = WolfHandler,
  public = list(
    #' @field description The file connection description
    description = ""
  ),
  private = list(
    connection = NULL,
    finalize = function() {
      if (self$active) {
        private$write_delayed()
      }
      if (
        inherits(private$connection, "connection") &&
          isOpen(connection) &&
          !connection %in% list(stdout(), stderr())
      ) {
        close(private$connection)
      }
    },
    writer = function(entry) {
      cat(format(entry), file = self$description)
    }
  )
)

#' File connection handler
#' 
#' @export
FileHandler <- R6::R6Class(
  "FileHandler",
  inherit = WolfHandler,
  public = list(
    #' @field path The file path
    path = NULL,

    #' @description Initialized the handler
    #' @param name Handler name
    #' @param path Path name.  When `NULL` resolves to a default name
    #' @param clear Removes file (if already exists)
    #' @param ... Additional arguments passed to `WolfHandler$initialize(...)`
    initialize = function(
      name, 
      path = NULL,
      clear = FALSE,
      ...
    ) {
      super$initialize(name, ...)
      path <- path %||% sprintf(".wolf-log-handler-%s%s", self$name, private$path_ext)
      self$path <- normalizePath(path, "/", mustWork = FALSE)
      if (clear && file.exists(self$path)) {
        file.remove(self$path)
      }
    }
  ),
  private = list(
    reader = function() {
      if (!file.exists(self$path)) {
        return(NULL)
      }
      readLines(self$path)
    },
    path_ext = ""
  )
)

#' Wolf default log handler
#' 
#' @export
LogHandler <- R6::R6Class(
  "WolfLogHandler",
  inherit = FileHandler,
  private = list(
    path_ext = ".log",
    writer = function(entry) {
      cat(
        trimws(format(entry)),
        "\n",
        sep = "",
        file = self$path,
        append = TRUE
      )
    }
  )
)

TableHandler <- R6::R6Class(
  "WolfTableHandler",
  inherit = FileHandler,
  public = list(
    initialize = function(
      name,
      path = NULL,
      type = c("csv", "tsv"),
      ...
    ) {
      type <- fuj::match_arg(type)
      private$path_ext <- paste0(".", type)
      super$initialize(name, path, ...)
      switch(
        type,
        "csv" = {
          private$active_reader <- private$reader_csv
          private$active_writer <- private$writer_csv
        },
        "tsv" = {
          private$active_reader <- private$reader_tsv
          private$active_writer <- private$writer_tsv
        },
        stop(fuj::internal_error())
      )

      private$queued_writer <- function(entries) {
        entries <- lapply(entries, \(e) fuj::quick_df(as.list(e)))
        entries <- Reduce(rbind, entries)
        private$writer(entries)
      }
    }
  ),
  private = list(
    writer = function(entry) {
      if (is.null(entry)) {
        return()
      }

      if (!is.data.frame(entry)) {
        entry <- fuj::quick_df(as.list(entry))
      }

      private$active_writer(entry)
    },
    active_writer = NULL,
    writer_csv = function(entry) {
      utils::write.table(
        entry,
        file = self$path,
        sep = ",",
        dec = ".",
        qmethod = "double",
        row.names = FALSE,
        col.names = !file.exists(self$path),
        na = ""
      )
    },
    writer_tsv = function(entry) {
      utils::write.table(
        entry,
        file = self$path,
        sep = "\t",
        dec = ".",
        qmethod = "double",
        row.names = FALSE,
        col.names = !file.exists(self$path)
      )
    },
    reader = function() {
      private$active_reader()
    },
    active_reader = NULL,
    reader_csv = function() {
      if (!file.exists(self$path)) {
        return(NULL)
      }
      utils::read.table(
        file = self$path,
        header = TRUE,
        sep = ",",
        quote= "\"",
        dec = ".",
        na.strings = ""
      )
    },
    reader_tsv = function() {
      if (!file.exists(self$path)) {
        return(NULL)
      }
      utils::read.table(
        file = self$path,
        header = TRUE,
        sep = "\t",
        quote= "\"",
        dec = ".",
        na.strings = ""
      )
    }
  )
)

DcfHandler <- R6::R6Class(
  "WolfDcfHandler",
  inherit = FileHandler,
  private = list(
    writer = function(entry) {
      write.dcf(
        as.list(entry),
        file = self$path,
        append = file.exists(self$path),
        indent = 2L,
        width = Inf,
      )
      cat("\n", file = self$path, append = TRUE)
    },
    reader = function() {
      if (!file.exists(self$path)) {
        return(NULL)
      }
      read.dcf(self$path)
    }
  )
)

# Not meant to be used externally
InternalHandler <- R6::R6Class(
  "WolfInternalHandler",
  inherit = DcfHandler,
  public = list(
    initialize = function(name = "wolf-internal", ...) {
      dir <- tools::R_user_dir("wolf", "cache")
      dir.create(dir, showWarnings = FALSE, recursive = TRUE)
      path <- file.path(dir, sprintf("%s.dcf", name))
      private$retain <- isTRUE(getOption("wolf.retain_internal_log", FALSE))
      super$initialize(name, path, clear = !private$retain, ...)
    }
  ),
  private = list(
    retain = NA,
    finalize = function() {
      if (!private$retain && file.exists(self$path)) {
          file.remove(self$path)
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

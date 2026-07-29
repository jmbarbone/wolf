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
    finalize = function() {
      if (self$active) {
        private$write_delayed()
      }
    }
  )
)

#' Connection handler
#' 
#' Write entries to a connection
#' 
#' @export
ConnectionHandler <- R6::R6Class(
  "WolfConnectionHandler",
  inherit = WolfHandler,
  public = list(
    #' @field connection The file connection
    connection = NULL,

    #' @description Initialized the handler
    #' @param name Handler name
    #' @param connection Connection to write to
    #' @param ... Additional arguments passed to `WolfHandler$initialize(...)`
    initialize = function(name, connection, ...) {
      stopifnot(inherits(connection, "connection"))
      super$initialize(name, ...)
      self$connection <- connection
    },

    #' @param entry A [wolf::wolf_entry()] object
    single_writer = function(entry) {
      cat(format(entry), file = self$connection)
    },

    #' @param entries A list of [wolf::wolf_entry()] objects
    bulk_writer = function(entries) {
      cat(fuj::vap_chr(entries, format), sep = "\n", file = self$connection)
    }
  ),
  private = list(
    finalize = function() {
      if (self$active) {
        private$write_delayed()
      }
      
      if (
        inherits(self$connection, "connection") &&
          isOpen(self$connection) &&
          !self$connection %in% list(stdout(), stderr())
      ) {
        close(self$connection)
      }
    }
  )
)

#' File connection handler
#' 
#' @export
FileHandler <- R6::R6Class(
  "WolfFileHandler",
  inherit = WolfHandler,
  public = list(
    #' @field path The file path
    path = NULL,

    #' @description Initialized the handler
    #' @param name Handler name
    #' @param path Path name.  When `NULL` resolves to a default name
    #' @param clear Removes file (if already exists)
    #' @param ... Additional arguments passed to `WolfHandler$initialize(...)`
    initialize = function(name, path = NULL, clear = FALSE, ...) {
      super$initialize(name, ...)
      path <- 
        path %||% 
        sprintf(".wolf-log-handler-%s%s", self$name, private$path_ext)
      self$path <- normalizePath(path, "/", mustWork = FALSE)
      if (clear && file.exists(self$path)) {
        file.remove(self$path)
      }
    },

    #' @description Read the log file
    read = function() {
      if (file.exists(self$path)) {
        readLines(self$path)
      }
    }
  ),
  private = list(path_ext = "")
)

#' Wolf default log handler
#' 
#' @export
LogHandler <- R6::R6Class(
  "WolfLogHandler",
  inherit = FileHandler,
  public = list(
    #' @description Writes a single [format()]'ed [wolf::wolf_entry()] object to
    #' the log file
    single_writer = function(entry) {
      cat(
        trimws(format(entry)),
        "\n",
        sep = "",
        file = self$path,
        append = TRUE
      )
    }
  ),
  private = list(path_ext = ".log")
)

#' Wolf table handler
#' 
#' @export
TableHandler <- R6::R6Class(
  "WolfTableHandler",
  inherit = FileHandler,
  public = list(
    #' @description Initializes the handler
    #' @param name Handler name
    #' @param path Path name.  When `NULL` resolves to a default name
    #' @param table_opts Options passed to [utils::write.table()]
    #' @param ... Additional arguments passed to `WolfHandler$initialize(...)`
    initialize = function(
      name,
      path = NULL,
      table_opts = list(),
      ...
    ) {
      super$initialize(name, path, ...)
      # TODO make safer
      private$table_opts <- modifyList(private$table_opts, table_opts)
    },

    #' @description Writes a single [wolf::wolf_entry()] object to the log file.
    #' This will append the `entry` as a new row
    #' @param entry A [wolf::wolf_entry()] object
    single_writer = function(entry) {
      utils::write.table(
        x = as.list(entry),
        file = self$path,
        sep = private$table_opts$sep,
        dec = private$table_opts$dec,
        qmethod = private$table_opts$qmethod,
        row.names = private$table_opts$row.names,
        col.names = FALSE,
        append = TRUE
      )
    },

    #' @description Writes a list of [wolf::wolf_entry()] objects to the log file.
    #' This will append the `entries` as new rows
    #' @param entries A list of [wolf::wolf_entry()] objects
    bulk_writer = function(entries) {
      entries <- lapply(entries, \(e) fuj::quick_df(as.list(e)))
      entries <- Reduce(rbind, entries)
      self$single_writer(entries)
    },

    #' @description Initializes the log file with column names
    init_writer = function() {
      writeLines(
        fuj::collapse(names(wolf_entry()), sep = private$table_opts$sep),
        self$path
      )
    },

    #' @description Reads the log file into a [base::data.frame] object
    read = function() {
      if (!file.exists(self$path)) {
        return(NULL)
      }
      args <- list(
        file = self$path,
        sep = private$table_opts$sep,
        dec = private$table_opts$dec,
        quote = switch(
          private$table_opts$qmethod,
          escape = "\"'",
          double = "\""
        ),
        header = TRUE
      )
      # this has to be 'missing' to work properly with read.table...
      if (private$table_opts$row.names) {
        args$row.names <- NULL
      }
      do.call(utils::read.table, args)
    }
  ),
  private = list(
    path_ext = ".txt",
    table_opts = list(
      # x = ,
      # file = "",
      # append = FALSE,
      quote = TRUE, 
      sep = " ",
      eol = "\n", 
      na = "", 
      dec = ".", 
      row.names = FALSE,
      qmethod = "escape", 
      fileEncoding = "UTF-8"
    )
  )
)

modify_opts <- function(...) {
  new <- list(...)
  old <- TableHandler$private_fields$table_opts
  stopifnot(names(new) %in% names(old))
  modifyList(old, new)
}

#' Wolf CSV handler
#' 
#' @export
CsvHandler <- R6::R6Class(
  "WolfCsvHandler",
  inherit = TableHandler,
  public = list(
    #' @description Initializes the handler
    #' @param name Handler name
    #' @param path Path name.  When `NULL` resolves to a default name
    #' @param ... Additional arguments passed to `WolfHandler$initialize(...)`
    initialize = function(name, path = NULL, ...) {
      table_opts <- list(sep = ",", qmethod = "double")
      super$initialize(name, path, table_opts, ...)
    }
  ),
  private = list(
    path_ext = ".csv",
    table_opts = modify_opts(
      sep = ",",
      qmethod = "double"
    )
  )
)

#' Wolf CSV2 handler
#' 
#' Writes entries to a CSV2 file (semicolon separated, comma decimal)
#' 
#' @export
Csv2Handler <- R6::R6Class(
  "WolfCsv2Handler",
  inherit = TableHandler,
  private = list(
    path_ext = ".csv2",
    table_opts = modify_opts(
      sep = ";", 
      dec = ",", 
      qmethod = "double"
    )
  )
)

#' Wolf TSV handler
#' 
#' @export
TsvHandler <- R6::R6Class(
  "WolfTsvHandler",
  inherit = TableHandler,
  private = list(
    path_ext = ".tsv",
    table_opts = modify_opts(
      sep = "\t",
      qmethod = "double"
    )
  )
)

Tsv2Handler <- R6::R6Class(
  "WolfTsv2Handler",
  inherit = TableHandler,
  private = list(
    path_ext = ".tsv2",
    table_opts = modify_opts(
      sep = "|",
      qmethod = "double"
    )
  )
)

DcfHandler <- R6::R6Class(
  "WolfDcfHandler",
  inherit = FileHandler,
  public = list(
    #' @description Writes a single [wolf::wolf_entry()] object to
    #' the log file
    single_writer = function(entry) {
      write.dcf(
        as.list(entry),
        file = self$path,
        append = file.exists(self$path),
        indent = 2L,
        width = Inf,
      )
      cat("\n", file = self$path, append = TRUE)
    },

    #' @description Writes a list of [wolf::wolf_entry()] objects to the log file.
    #' @param entries A list of [wolf::wolf_entry()] objects
    bulk_writer = function(entries) {
      self$single_writer(lapply(entries, as.list))
    },

    #' @description Reads the log file into a [base::matrix] object
    read = function() {
      if (file.exists(self$path)) {
        read.dcf(self$path)
      }
    }
  ),
  private = list(path_ext = ".dcf")
)

#' JSON handler
#'
#' File is overwritten each time with only the most recent entry.  At finalize,
#' all entries are written to the file. [wolf::JsonlHandler] is recommended for 
#' continuous writing.
#'  
#' @export
JsonHandler <- R6::R6Class(
  "WolfJsonHandler",
  inherit = FileHandler,
  public = list(
    #' @description Writes a single [wolf::wolf_entry()] object to
    #' the log file
    #' @param entry A [wolf::wolf_entry()] object
    single_writer = function(entry) {
      jsonlite::write_json(as.list(entry), self$path, auto_unbox = TRUE)
    },

    #' @description Writes a list of [wolf::wolf_entry()] objects to the log file.
    #' @param entries A list of [wolf::wolf_entry()] objects
    bulk_writer = function(entries) {
      self$single_writer(lapply(entries, as.list))
    },

    #' @description Reads the log file into a [base::list] object
    #' @return A list of [wolf::wolf_entry()] objects
    reader = function() {
      if (!file.exists(self$path)) {
        return(NULL)
      }
      jsonlite::fromJSON(self$path)
    }
  ),
  private = list(
    path_ext = ".json",
    finalize = function() {
      if (self$active) {
        # save over all entries
        private$bulk_writer(private$entries)
      }
    }
  )
)

#' JSONL handler
#' 
#' Each entry is written as a single line of JSON.  This is recommended over the
#' [wolf::JsonHandler] for continuous writing
#' 
#' @export
JsonlHandler <- R6::R6Class(
  "WolfJsonlHandler",
  inherit = FileHandler,
  public = list(
    #' @description Writes a single [wolf::wolf_entry()] object to
    #' the log file
    #' @param entry A [wolf::wolf_entry()] object
    single_writer = function(entry) {
      json <- jsonlite::toJSON(as.list(entry), auto_unbox = TRUE)
      cat(json, "\n", file = self$path, append = TRUE)
    },

    #' @description Writes a list of [wolf::wolf_entry()] objects to the log file.
    #' @param entries A list of [wolf::wolf_entry()] objects
    bulk_writer = function(entries) {
      for (entry in entries) {
        self$single_writer(entry)
      }
    },

    #' @description Reads the log file into a [base::list] object
    #' @return A list of [wolf::wolf_entry()] objects
    reader = function() {
      if (!file.exists(self$path)) {
        return(NULL)
      }

      lapply(readLines(self$path), jsonlite::fromJSON)
    }
  ),
  private = list(path_ext = ".jsonl")
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

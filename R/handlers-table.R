#' Wolf table handler
#'
#' Write entries to a table file (CSV, TSV, etc.)
#'
#' @description
#' - [TableHandler] writes entries to a table file (space separated, dot
#'   decimal); default
#' - [CsvHandler] writes entries to a CSV file (comma separated, dot decimal)
#' - [Csv2Handler] writes entries to a CSV2 file (semicolon separated, comma
#'   decimal)
#' - [TsvHandler] writes entries to a TSV file (tab separated, dot decimal)
#' - [Tsv2Handler] writes entries to a TSV2 file (pipe separated, dot decimal)
#'
#'
#' @md
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

    #' @description Writes a list of [wolf::wolf_entry()] objects to the log
    #'   file. This will append the `entries` as new rows
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
  utils::modifyList(old, new)
}

#' @export
#' @rdname TableHandler
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

#' @export
#' @rdname TableHandler
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

#' @export
#' @rdname TableHandler
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

#' @export
#' @rdname TableHandler
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

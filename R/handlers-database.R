#' DBI Handler
#'
#' A handler generic handler for DBI connections.
#'
#' @description
#' This uses `DBI::dbSendQuery()` and `DBI::dbWriteTable()` in the background
#'
#' @param connection A database connection object
#' @export
DbiHandler <- R6::R6Class(
  "DbiHandler",
  inherit = WolfHandler,
  public = list(
    #' @field connection A database connection object
    connection = NULL,

    #' @field table The name of the table to write to
    table = NULL,

    #' @description Initialized object
    #' @param name name identifier
    #' @param connection A database connection object
    #' @param table The name of the table to write to
    #' @param ... Additional arguments (not used)
    initialize = function(name, connection, table, ...) {
      requireNamespace("DBI")
      self$connection <- connection
      self$table <- table
      super$initialize(name, ...) # very highly recommended
    },

    #' @description Initializes the database table if it does not exist
    init_writer = function() {
      DBI::dbSendQuery(
        self$connection,
        sprintf(dbi_generic_create_table_template, self$table)
      )
    },

    #' @description Writes a single [wolf::wolf_entry()] object to the database.
    #' @param entry A [wolf::wolf_entry()] object
    single_writer = function(entry) {
      df <- as.data.frame(entry)
      DBI::dbWriteTable(self$connection, self$table, df, append = TRUE)
    },

    #' @description Writes multiple [wolf::wolf_entry()] objects to the database.
    #' @param entries A list of [wolf::wolf_entry()] objects
    bulk_writer = function(entries) {
      df <- do.call(rbind, lapply(entries, as.data.frame))
      DBI::dbWriteTable(self$connection, self$table, df, append = TRUE)
    },

    #' @description Read the log table
    read = function() {
      df <- DBI::dbReadTable(self$connection, self$table)
      df$type <- ordered(df$type, c("debug", "info", "warn", "error"))
      df$expr <- lapply(df$expr, \(e) if (!is.null(e)) str2expression(e))
      df
    }
  )
)

# TODO this could be added to the class or something, and maybe passed into
# some function?  This is probably good as a default for now
dbi_generic_create_table_template <- "
CREATE TABLE IF NOT EXISTS %s (
  time TIMESTAMP,
  type STRING,
  name STRING,
  message STRING,
  frame STRING,
  expr STRING,
  condition STRING,
  id STRING
)"

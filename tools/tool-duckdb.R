# Example of a custom handler, for DuckDB

require(wolf)
library(R6)
library(duckdb)

DuckDbHandler <- R6Class(
  "DuckDbHandler",
  inherit = WolfHandler,
  public = list(
    connection = NULL,
    table = NULL,
    initialize = function(name, connection, table, ...) {
      stopifnot(inherits(connection, "duckdb_connection"))
      super$initialize(name, ...) # very highly recommended
      self$connection <- connection
      self$table <- table
    },
    init_writer = function() {
      DBI::dbSendQuery(
        con,
        sprintf(
          "CREATE TABLE IF NOT EXISTS %s (
            time TIMESTAMP,
            type TEXT,
            name TEXT,
            message TEXT,
            frame TEXT,
            expr TEXT,
            condition TEXT,
            id TEXT
          )",
          self$table
        )
      )
    },
    single_writer = function(entry) {
      df <- as.data.frame(entry)
      DBI::dbWriteTable(self$connection, self$table, df, append = TRUE)
    },
    bulk_writer = function(entries) {
      df <- do.call(rbind, lapply(entries, as.data.frame))
      DBI::dbWriteTable(self$connection, self$table, df, append = TRUE)
    },
    read = function() {
      df <- DBI::dbReadTable(self$connection, self$table)
      df$type <- ordered(df$type, c("debug", "info", "warn", "error"))
      df$expr <- lapply(
        df$expr,
        function(e) {
          if (is.null(e)) {
            return(NULL)
          }
          str2expression(e)
        }
      )
      if (requireNamespace("tibble", quietly = TRUE)) {
        df <- tibble::as_tibble(df)
      }
      df
    }
  )
)

# ":memory:"
con <- DBI::dbConnect(duckdb(), dbdir = ":memory:")
duck_db_handler <- DuckDbHandler$new("duckdb", con, "wolf")

w <- wolf()
w$add_handler(duck_db_handler)
w$info("This is an info message")
w$warn("This is a warning message")

print(duck_db_handler$read())

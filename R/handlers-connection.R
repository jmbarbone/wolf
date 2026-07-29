#' @title Connection handler
#'
#' @description Writes entries to a connection
#'
#' @include handlers.R
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
    initialize = function(name, connection = stdout(), ...) {
      stopifnot(inherits(connection, "connection"))
      super$initialize(name, ...)
      self$connection <- connection
    },

    #' @description Writes a single [wolf::wolf_entry()] object to the connection
    #' @param entry A [wolf::wolf_entry()] object
    single_writer = function(entry) {
      cat(format(entry), file = self$connection)
    },

    #' @description Writes a list of [wolf::wolf_entry()] objects to the connection
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

#' @title Connection handler
#'
#' @description Writes entries to a connection
#'
#' @include handlers.R
#' @export
ConnectionHandler <- R6::R6Class(
  "ConnectionHandler",
  inherit = WolfHandler,
  public = list(
    #' @field connection The file connection
    connection = NULL,

    #' @description Initialized the handler
    #' @param name Handler name
    #' @param connection Connection to write to
    #' @param ... Additional arguments passed to `WolfHandler$initialize(...)`
    initialize = function(name, connection = file(), ...) {
      stopifnot(inherits(connection, "connection"))
      super$initialize(name, ...)
      self$connection <- connection
    },

    #' @description Writes a single [wolf::wolf_entry()] object to the
    #'   connection
    #' @param entry A [wolf::wolf_entry()] object
    single_writer = function(entry) {
      cat(format(entry), file = self$connection)
    },

    #' @description Writes a list of [wolf::wolf_entry()] objects to the
    #'   connection
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
          tryCatch(isOpen(self$connection), error = function(e) FALSE) &&
          !self$connection %in% list(stdout(), stderr())
      ) {
        close(self$connection)
      }
    }
  )
)

#' Stdout Handler
#'
#' @export
StdoutHandler <- R6::R6Class(
  "StdoutHandler",
  inherit = ConnectionHandler,
  public = list(
    #' @description Initializes the handler
    #' @param name Handler name
    #' @param ... Additional arguments passed to
    #'   `ConnectionHandler$initialize(...)`
    initialize = function(name, ...) {
      super$initialize(name, stdout(), ...)
    }
  )
)

#' Stderr Handler
#'
#' @export
StderrHandler <- R6::R6Class(
  "StderrHandler",
  inherit = ConnectionHandler,
  public = list(
    #' @description Initializes the handler
    #' @param name Handler name
    #' @param ... Additional arguments passed to
    #'   `ConnectionHandler$initialize(...)`
    initialize = function(name, ...) {
      super$initialize(name, stderr(), ...)
    }
  )
)

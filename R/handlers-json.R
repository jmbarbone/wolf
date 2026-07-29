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
    validate = function() {
      fuj::require_namespace("jsonlite")
    },
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
  private = list(
    path_ext = ".jsonl",
    validate = function() {
      fuj::require_namespace("jsonlite")
    }
  )
)

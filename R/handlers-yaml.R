
#' Wolf Yaml Handler
#' 
#' Like the [wolf::JsonHandler], the `$single_writer()` method overwrites the
#' log with the most recent value; and all entries are written when closed.
#' 
#' @include handlers-file.R
#' @export
YamlHandler <- R6::R6Class(
  "WolfYamlHandler",
  inherit = FileHandler,
  public = list(
    #' @description Initializes the handler
    #' @param name Handler name
    #' @param ... Additional arguments passed to `WolfHandler$initialize(...)`
    initialize = function(name, ...) {
      super$initialize(name, ...)
    },
    #' @description Writes a single [wolf::wolf_entry()] object to the log file
    #' @param entry A [wolf::wolf_entry()] object
    single_writer = function(entry) {
      yaml::write_yaml(as.list(entry), self$path)
    },

    #' @description Writes a list of [wolf::wolf_entry()] objects to the log file.
    #' @param entries A list of [wolf::wolf_entry()] objects
    bulk_writer = function(entries) {
      yaml::write_yaml(lapply(entries, as.list), self$path)
    },

    #' @description Reads the log file into a [base::list] object
    read = function() {
      if (file.exists(self$path)) {
        yaml::read_yaml(self$path)
      }
    }
  ),
  private = list(
    path_ext = ".yml",
    validate = function() {
      fuj::require_namespace("yaml")
    },
    finalize = function() {
      if (self$active) {
        # save over all entries
        private$bulk_writer(private$entries)
      }
    }
  )
)

#' Wolf Yaml 1.2 Handler
#'
#' Like the [wolf::JsonlHandler], the `$single_writer()` method appends each 
#' entry to the log file.
#' 
#' @include handlers-file.R
#' @export
Yaml12Handler <- R6::R6Class(
  "WolfYaml12Handler",
  inherit = FileHandler,
  public = list(
    #' @description Initializes the handler
    #' @param name Handler name
    #' @param ... Additional arguments passed to `WolfHandler$initialize(...)`
    initialize = function(name, ...) {
      super$initialize(name, ...)
    },

    #' @description Writes a single [wolf::wolf_entry()] object to the log file
    #' @param entry A [wolf::wolf_entry()] object
    single_writer = function(entry) {
      yaml <- yaml12::format_yaml(as.list(entry))
      cat("---", yaml, sep = "\n", file = self$path, append = TRUE)
    },

    #' @description Writes a list of [wolf::wolf_entry()] objects to the log file.
    #' @param entries A list of [wolf::wolf_entry()] objects
    bulk_writer = function(entries) {
      yaml <- yaml12::format_yaml(lapply(entries, as.list), multi = TRUE)
      cat(yaml, file = self$path, append = TRUE)
    },

    #' @description Reads the log file into a [base::list] object
    read = function() {
      if (file.exists(self$path)) {
        yaml12::read_yaml(self$path, multi = TRUE)
      }
    }
  ),

  private = list(
    path_ext = ".yaml",
    validate = function() {
      fuj::require_namespace("yaml12")
    }
  )
)

#' @title Wolf File handler
#'
#' @description Writes to a flat file
#'
#' @include handlers.R
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

#' Wolf DCF handler
#'
#' @export
DcfHandler <- R6::R6Class(
  "WolfDcfHandler",
  inherit = FileHandler,
  public = list(
    #' @description Writes a single [wolf::wolf_entry()] object to
    #' the log file
    #' @param entry A [wolf::wolf_entry()] object
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

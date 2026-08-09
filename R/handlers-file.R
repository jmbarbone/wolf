#' @title Wolf File handler
#'
#' @description Writes to a flat file
#'
#' @include handlers.R
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
    #' @param overwrite Overwrites the file if it exists
    #' @param cleanup Removes file when handler is finalized
    #' @param ... Additional arguments passed to `WolfHandler$initialize(...)`
    initialize = function(
      name,
      path = NULL,
      overwrite = TRUE,
      cleanup = FALSE,
      ...
    ) {
      super$initialize(name, ...)
      private$cleanup <- cleanup
      private$overwrite <- overwrite
      path <-
        path %||%
        sprintf(".wolf-log-handler-%s%s", self$name, private$path_ext)
      self$path <- normalizePath(path, "/", mustWork = FALSE)
      dir.create(dirname(self$path), recursive = TRUE, showWarnings = FALSE)
      if (private$overwrite && file.exists(self$path)) {
        file.remove(self$path)
      }
    },

    #' @description Writes a single [wolf::wolf_entry()] object to the log file.
    #' @param entry A [wolf::wolf_entry()] object
    single_writer = function(entry) {
      cat(
        format(entry),
        "\n",
        sep = "",
        file = self$path,
        append = TRUE
      )
    },

    #' @description Read the log file
    read = function() {
      if (file.exists(self$path)) {
        readLines(self$path)
      }
    }
  ),
  private = list(
    path_ext = "",
    cleanup = FALSE,
    overwrite = TRUE,
    finalize = function() {
      if (self$active) {
        private$write_delayed()
      }

      if (private$cleanup && file.exists(self$path)) {
        file.remove(self$path)
      }
    }
  )
)

#' Wolf default log handler
#'
#' @export
LogHandler <- R6::R6Class(
  "LogHandler",
  inherit = FileHandler,
  public = list(
    #' @field format_method Format method
    format_method = eval(formals(format.wolf_entry)$method),

    #' @description Initializes the handler
    #' @param format Format method.  One of `"full"`, `"short"`, or `"json"`.
    #' @param name,... Additional arguments passed to
    #'   `FileHandler$initialize(...)`
    initialize = function(name, format_method = self$format_method, ...) {
      self$format_method <- fuj::match_arg(format_method)
      super$initialize(name, ...)
    },

    #' @description Writes a single [format()]'ed [wolf::wolf_entry()] object to
    #' the log file
    single_writer = function(entry) {
      cat(
        format.wolf_entry(entry, self$format),
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
  "DcfHandler",
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

    #' @description Writes a list of [wolf::wolf_entry()] objects to the log
    #'   file.
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

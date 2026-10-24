# write(entry) should then handle everything within the entry
# close() should be used to handle anything

# public methods should not be modified

#' WolfHandler
#'
#' Base [R6::R6Class()] for all handlers.
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

    #' @field queued A list of [wolf::wolf_entry()] objects that have not yet
    #'   been written
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
    #' @param delay Whether entries should be written immediately or not.
    #' @description Initialized object
    initialize = function(name, ..., delay = FALSE) {
      self$name <- name
      self$delay <- delay
      private$validate()
      self$init_writer()
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
        self$write(entry)
      }

      self
    },

    #' @param entry A [wolf::wolf_entry()] object
    #' @description Calls the private writer method.  When `sel$delay = FALSE`,
    #' and there are entries in `self$queued`, each file is written.  To control
    #' _how_ the entries are written, modify the `$writer()` method
    write = function(entry) {
      private$write_delayed()
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
    init_writer = function() {
      NULL
    },

    #' @description Read lines
    #' @description Read entries
    read_lines = function() {
      readLines(
        self$path %||% self$connection,
        warn = FALSE,
        encoding = "UTF-8",
        skipNul = TRUE
      )
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
        self$bulk_writer(private$queue_)
        private$queue_ <- NULL
      }
    },
    # modifiable
    validate = function() {
      # check args
      private$validate_writer(self$single_writer, "single")
      private$validate_writer(self$bulk_writer, "bulk")
    },
    validate_writer = function(fun, method) {
      if (is.null(fun)) {
        return()
      }

      if (!is.function(fun)) {
        stop("writer must be a function", call. = FALSE)
      }

      switch(
        method,
        single = {
          if (!identical(formals(fun), as.pairlist(alist(entry = )))) {
            warning(
              "writer must have a single argument named 'entry'",
              " for writer: ",
              deparse(substitute(fun)),
              " in class ",
              class(self)[1L],
              call. = FALSE
            )
          }
        },
        bulk = {
          if (!identical(formals(fun), as.pairlist(alist(entries = )))) {
            warning(
              "writer must have a single argument named 'entries'",
              " for writer: ",
              deparse(substitute(fun)),
              " in class ",
              class(self)[1L],
              call. = FALSE
            )
          }
        }
      )
    },
    finalize = function() {
      if (self$active) {
        private$write_delayed()
      }
    }
  )
)

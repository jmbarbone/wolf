WolfHanlder <- R6::R6Class(
  "WolfHandler",
  public = list(
    name = NULL,
    process = function() {
      if (!self$active) {
        return(self)
      }

      if (!self$queued) {
        return(self)
      }

      self$handle(entry)
      self
    }
    handle = function(entry) NULL,
    write = function(entry) NULL,
    close = function() {
      tryCatch(private$finalize(), error = )
    }
  )
)

new_handler <- function(name, write = NULL, close = NULL, ...) {
  public <- list(write = write, close = close)
  public <- public[!vapply(public, is.null, NA)]
  R6::R6Class(name, inherit = WolfHanlder, public = public)
}

ConnectionHandler <- R6::R6Class(
  "WolfConnectionHandler",
  inherit = WolfHanlder,
  public = list(
    close = function() {
      if (
        isinstance(private$con, "connection") &&
        isOpen(con) &&
        !con %in% list(stdout(), stderr())
      ) {
        close(private$con)
      }
    },
    handle = function(entry) {
      cnd <- entry$condition
      entry <- private$make_entry(cnd)
      private$writer(entry)
    }
  ),
  private <- list(
    con = NULL,
    make_entry = function() NULL,
    writer = function() NULL,
  )
)

LogHandler <- R6::R6Class(
  "WolfLogHandler",
  inherit = WolfConnectionHandler,
  private = list(
    make_entry = function(cnd) {
      list(
        time = Sys.time(),
        type = get_type(cnd),
        name = self$name,
        message = trimws(conditionMessage(cnd))
      )
    },
    writer = function(entry) {
      msg <- sprintf(
        "%s [%s] (%s) %s\n",
        format(entry$time, "%Y-%m-%d %H:%M:%S"),
        entry$type,
        entry$name,
        paste0(entry$message, collapse = "")
      )

      msg <- paste(msg, collapse = "")
      msg <- trimws(msg)
      msg <- paste0(msg, "\n")
      cat(msg, file = private$con)
    }
  )
)

TableHandler <- R6::R6Class(
  "WolfTableHandler",
  inherit = WolfConnectionHandler,
  public = list(
    initialize = function(
      name,
      con,
      type = c("csv", "tsv", "dcf")
    ) {
      self$name <- name
      self$writer <- switch(
        match.arg(type),
        csv = function(entry) {
          utils::write.table(
            fuj::quick_df(entry),
            file = private$con,
            sep = ",",
            dec = ".",
            qmethod = "double",
            row.names = FALSE,
            col.names = !file.exists(private$con)
          )
        },
        tsv = function(entry) {
          utils::write.table(
            fuj::quick_df(entry),
            file = private$con,
            sep = "\t",
            dec = ".",
            qmethod = "double",
            row.names = FALSE,
            col.names = !file.exists(private$con)
          )
        },
        dcf = function(entry) {
          write.dcf(
            entry,
            file = private$con,
            append = TRUE,
            indent = 2L,
            width = Inf,
          )
          cat("\n", file = private$con)
        },
         stop("unknown table type:", type)
      )
    }
  ),
  private = list(
    make_entry = function(cnd) {
      list(
        time = Sys.time(),
        type = get_type(cnd),
        name = self$name,
        message = trimws(conditionMessage(cnd))
      )
    }
  )
)



#' Wolf default handlers
#'
#' @param name Name of the handler.
#' @param path Path to use for file-based handlers.
#' @param con Connection to use for connection-based handlers.
#' @param cnd Condition to handle.
#' @name wolf_handlers
NULL

wolf_handler <- function(
  name,
  handler,
  path = NULL,
  reader = NULL,
  formatter = NULL
) {
  structure(
    list(
      name = name,
      handler = handler,
      path = path,
      reader = reader,
      formatter = formatter
    ),
    class = c("wolf_handler", "list")
  )
}

c("keepNA", "keepInteger", "niceNames")

#' @export
print.wolf_handler <- function(x, ...) {
  fun <- utils::capture.output(utils::str(x$handler))
  fun <- paste0(fun, collapse = " ")
  cat("<wolf handler>\n")
  cat("  name:     ", x$name, "\n", sep = "")
  cat("  handler:  ", fun, "\n", sep = "")
  cat("  path:     ", x$path %||% "<>", "\n", sep = "")
  cat("  reader:   ", x$reader %||% "<>", "\n", sep = "")
  cat("  formatter:", x$formatter %||% "<>", "\n", sep = "")
}

wolf_handler_spec <- list(
  handler = "some_function",
  path = NULL,
  reader = "some_function",
  formatter = "some_function"
)

#' @rdname wolf_handlers
#' @export
wolf_default_handler <- function(cnd) {
  switch(
    get_type(cnd),
    message = message(cnd),
    warning = warning(cnd),
    error = stop(cnd),
    cat(conditionMessage(cnd), "\n", sep = "")
  )
}

#' @rdname wolf_handlers
#' @export
wolf_default_con_handler <- function(name, con) {
  function(cnd) {
    msg <- sprintf(
      "%s [%s] (%s) %s\n",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      get_type(cnd),
      name,
      paste0(conditionMessage(cnd), collapse = "")
    )

    msg <- paste(msg, collapse = "")
    msg <- trimws(msg)
    msg <- paste0(msg, "\n")
    cat(msg, file = con)
  }
}

#' @rdname wolf_handlers
#' @export
wolf_default_writer_log <- function(name, path) {
  .name <- name
  .path <- default_writer_setup(name, path, "log")
  function(cnd) {
    f <- file(.path, "a+b", encoding = "UTF-8")
    on.exit(close(f), add = TRUE)

    msg <- sprintf(
      "%s [%s] (%s) %s\n",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      get_type(cnd),
      .name,
      paste0(conditionMessage(cnd), collapse = "")
    )

    msg <- paste(msg, collapse = "")
    msg <- trimws(msg)
    msg <- paste0(msg, "\n")
    msg <- charToRaw(msg)
    writeBin(msg, f)
  }
}

#' @rdname wolf_handlers
#' @export
wolf_default_writer_csv <- function(name, path) {
  .name <- name
  .path <- default_writer_setup(name, path, "csv.log")
  function(cnd) {
    exists <- file.exists(.path)
    f <- file(.path, "ab", encoding = "UTF-8")
    on.exit(close(f), add = TRUE)
    utils::write.table(
      fuj::quick_df(list(
        time = Sys.time(),
        type = get_type(cnd),
        name = .name,
        message = trimws(conditionMessage(cnd))
      )),
      file = f,
      sep = ",",
      dec = ".",
      qmethod = "double",
      row.names = FALSE,
      append = exists,
      col.names = !exists
    )
  }
}

#' @rdname wolf_handlers
#' @export
wolf_default_writer_tsv <- function(name, path) {
  .name <- name
  .path <- default_writer_setup(name, path, "tsv.log")
  function(cnd) {
    exists <- file.exists(.path)
    f <- file(.path, "ab", encoding = "UTF-8")
    on.exit(close(f), add = TRUE)
    utils::write.table(
      fuj::quick_df(list(
        time = Sys.time(),
        type = get_type(cnd),
        name = .name,
        message = trimws(conditionMessage(cnd))
      )),
      file = f,
      sep = "\t",
      append = exists,
      col.names = !exists,
      row.names = FALSE,
    )
  }
}

#' @rdname wolf_handlers
#' @export
wolf_default_writer_dcf <- function(name, path) {
  .path <- default_writer_setup(name, path, "dcf.log")
  .name <- name
  function(cnd) {
    f <- file(.path, "ab", encoding = "UTF-8")
    on.exit(close(f), add = TRUE)
    write.dcf(
      list(
        time = Sys.time(),
        type = get_type(cnd),
        name = .name,
        message = trimws(conditionMessage(cnd))
      ),
      file = f,
      append = TRUE,
      indent = 2L,
      width = Inf,
    )
    cat("\n", file = f)
  }
}

default_writer_setup <- function(name, path, ext = "log") {
  dir.create(path, FALSE, TRUE)
  name <- sprintf("wolf-%s.%s", name, ext)
  print(normalizePath(file.path(path, name), "/", FALSE))
}

# TODO include a log_entry class
# TODO Wolf$entries should return a list[log_entry]
# TODO maybe add an option to only write on command or i
#

wolf_entries <- function(...) {
  x <- list(...)
  class(x) <- c("wolf_entries", "list")
  x
}

wolf_entry <- function(
  name = NULL,
  message = NULL,
  time = Sys.time(),
  type = c("debug", "info", "warn", "error"),
  call = NULL,
  frame = NULL,
  expr = NULL,
  condition = NULL
) {
  type <- fuj::match_arg(type)
  x <- list(
    time = time,
    type = type,
    name = name,
    message = message,
    frame = frame,
    call = call,
    expr = expr,
    condition = condition
  )
  x$id <- mark::md5(x)
  class(x) <- c("wolf_entry", "list")
  x
}

#' @export
print.wolf_entries <- function(x, method = c("default", "compact"), ...) {
  for (i in x) {
    print(i, method = method)
  }
  invisible(x)
}

#' @export
print.wolf_entry <- function(x, method = c("default", "compact"), ...) {
  method <- fuj::match_arg(method)

  switch(
    method,
    #
    default = cat(
      cli::col_silver("time: "),
      cli::col_blue(format(x$time, usetz = TRUE)),
      "\n",
      cli::col_silver("type: "),
      cli::col_green(x$type),
      "\n",
      cli::col_silver("name: "),
      cli::col_yellow(x$name),
      "\n",
      cli::col_silver("frame: "),
      if (!is.null(x$frame)) cli::col_grey(format(x$frame)),
      "\n",
      cli::col_silver("call: "),
      if (!is.null(x$call)) cli::col_grey(format(x$call)),
      "\n",
      x$message,
      "\n",
      sep = ""
    ),
    compact = cat(
      cli::col_blue(format(x$time, "%Y-%m-%d %H:%M:%S")),
      " ",
      cli::col_green(x$type),
      " ",
      if (!is.null(x$name)) {
        c(cli::col_yellow(x$name), " ")
      },
      x$message,
      "\n",
      sep = ""
    )
  )
  invisible(x)
}

#' @export
format.wolf_entry <- function(x, ...) {
  sprintf(
    "%s [%s] (%s) %s",
    format(x$time, "%Y-%m-%d %H:%M:%S"),
    x$type,
    x$name %||% "",
    paste0(x$message %||% "", collapse = "")
  )
}

#' @export
as.list.wolf_entry <- function(x, ...) {
  x <- unclass(x)
  x$message <- x$message %||% ""
  x$name <- x$name %||% ""
  x$condition <- x$condition %||% ""
  x$call <- format(x$call %||% NA)
  x$expr <- format(x$expr %||% NA)
  x$frame <- format(x$frame %||% NA)
  x
}

# TODO include a log_entry class
# TODO Wolf$entries should return a list[log_entry]
# TODO maybe add an option to only write on command or i
#

#' Wolf entry object
#'
#' @param name The name of the entry
#' @param message The message of the entry
#' @param time The time of the entry
#' @param type The type of the entry
#' @param frame The frame of the entry
#' @param expr The expression of the entry
#' @param condition The condition of the entry
#'
#' @export
wolf_entry <- function(
  name = NULL, # character()
  message = NULL, # character()
  time = Sys.time(),
  type = c("debug", "info", "warn", "error"),
  frame = parent.frame(),
  expr = NULL, # expression()
  condition = NULL # character()
) {
  type <- fuj::match_arg(type)
  x <- list(
    time = time,
    type = ordered(type, c("debug", "info", "warn", "error")),
    name = name,
    message = message,
    frame = frame,
    expr = expr,
    condition = condition
  )
  x$id <- md5(x)
  class(x) <- c("wolf_entry", "list")
  x
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
format.wolf_entry <- function(x, method = c("partial", "full"), ...) {
  method <- fuj::match_arg(method)
  switch(
    method,
    partial = sprintf(
      "%s [%s] (%s) %s",
      format(x$time, "%Y-%m-%d %H:%M:%S"),
      x$type,
      x$name,
      paste0(x$message %||% "", collapse = "")
    ),
    full = sprintf(
      "%s [%s] (%s) %s `%s` %s",
      format(x$time, "%Y-%m-%d %H:%M:%S"),
      x$type,
      x$name %||% "",
      paste0(x$message %||% "", collapse = ""),
      format(x$expr %||% NA),
      format(x$frame %||% NA)
    )
  )
}

#' @export
as.list.wolf_entry <- function(x, ...) {
  x <- unclass(x)
  x$message <- x$message %||% ""
  x$name <- x$name %||% ""
  x$condition <- x$condition %||% ""
  x$expr <- format(x$expr %||% NA)
  x$frame <- format(x$frame %||% NA)
  x
}

#' @export
as.data.frame.wolf_entry <- function(x, ...) {
  mark::quick_df(as.list(x))
}

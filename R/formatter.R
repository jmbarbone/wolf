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
  type = c("info", "warn", "error", "debug", "info")
) {
  type <- match.arg(type)
  x <- list(
    time = time,
    type = type,
    name = name,
    message = message
  )
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
  method <- match.arg(method)

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
      x$message,
      "\n",
      sep = ""
    ),
    compact = cat(
      cli::col_blue(format(x$time, "%Y-%m-%d %H:%M:%S")),
      cli::col_green(x$type),
      cli::col_yellow(x$name),
      x$message,
      "\n",
      sep = ""
    )
  )
  invisible(x)
}

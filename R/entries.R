# TODO include a log_entry class
# TODO Wolf$entries should return a list[log_entry]
# TODO maybe add an option to only write on command or i
#

wolf_entries <- function(x) {
  class(x) <- c("wolf_entries", "list")
  x
}

wolf_entry <- function(
  time = .POSIXct(integer()),
  type = character(),
  name = character(),
  message = character()
) {
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
print.wolf_entries <- function(x, ...) {
  for (i in seq_along(x)) {
    if (i != 1L) {
      cat(cli::cli_rule(), "\n")
      print(x)
    }
  }
  invisible(x)
}

#' @export
print.wolf_entry <- function(x, ...) {
  cat(
    cli::col_blue(format(x$time, "%Y-%m-%d %H:%M:%S")),
    cli::col_green(x$type),
    cli::col_yellow(x$name),
    "\n",
    x$message,
    "\n"
  )
  invisible(x)
}

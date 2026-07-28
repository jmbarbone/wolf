#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# nolint next: object_name_linter.
op.wolf <- list(
  wolf.retain_internal_log = FALSE
)

internal <- NULL
internal_handler <- NULL

.onLoad <- function(libname, pkgname) {
  options(op.wolf[names(op.wolf) %notin% names(options())])
  internal <<- wolf(name = "wolf-internal", level = "debug")
  internal_handler <<- InternalHandler$new()
  internal$add_handler(internal_handler)
  internal$debug("wolf-internal logger initialized")
}

.onUnload <- function(libname) {
  internal$debug("wolf-internal logger shutting down")
}

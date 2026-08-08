#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# nolint next: object_name_linter.
op.wolf <- list(
  wolf..internal.logger = FALSE,
  wolf..internal.logger.retain = TRUE
)

internal <- NULL
internal_handler <- NULL

.onLoad <- function(libname, pkgname) {
  options(op.wolf[names(op.wolf) %notin% names(options())])
  if (!getOption("wolf..internal.logger", FALSE)) {
    return(invisible())
  }
  internal <<- wolf(name = "wolf-internal", level = "debug")
  retain_internal_log <- getOption("wolf..internal.logger.retain", FALSE)
  internal_handler <<- LogHandler$new(
    name = "wolf-internal",
    path = file.path(tools::R_user_dir("wolf", "cache"), "wolf-internal.log"),
    overwrite = retain_internal_log,
    cleanup = !retain_internal_log
  )
  internal$add_handler(internal_handler)
  internal$debug("wolf-internal logger initialized")
}

.onUnload <- function(libname) {
  if (inherits(internal, "Wolf")) {
    internal$debug("wolf-internal logger shutting down")
  }
}

.force_namespace_imports <- function() {
  # okay, so like, R CMD check thinks I'm not using R6...
  R6::R6Class
}

get_type <- function(cnd) {
  if (inherits(cnd, "message")) {
    "message"
  } else if (inherits(cnd, c("warning", "warning"))) {
    "warning"
  } else if (inherits(cnd, "error")) {
    "error"
  } else if (inherits(cnd, "debug")) {
    "debug"
  } else if (inherits(cnd, "info")) {
    "info"
  } else {
    "condition"
  }
}

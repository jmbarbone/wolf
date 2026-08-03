# from mark::md5()
md5 <- function(x) {
  s <- serialize(x, NULL, xdr = TRUE)
  if (getRversion() >= "4.5.0") {
    return(tools::md5sum(bytes = s))
  }
  path <- tempfile()
  on.exit(file.remove(path), add = TRUE)

  con <- file(path, "wb", encoding = "UTF-8", raw = TRUE)
  on.exit(close(con), add = TRUE)

  writeBin(s, con, useBytes = TRUE)
  tools::md5sum(path)
}

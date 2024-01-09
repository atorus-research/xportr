.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(xportr_options) %in% names(op))
  if (any(toset)) options(xportr_options[toset])

  invisible()
}

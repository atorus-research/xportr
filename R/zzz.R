.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    xportr.coerse = "none"
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])
  
  invisible()
}
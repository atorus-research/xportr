#' The `xportr` package
#'
#' Package Info here
#'
#' @importFrom purrr map_chr walk2
#' @importFrom dplyr left_join
#' @importFrom glue glue
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    xportr.coerse = "none"
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}

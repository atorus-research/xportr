#' The `xportr` package
#'
#' Package Info here
#'
#' @keywords internal
#'
#' @import rlang
#' @importFrom purrr map_chr walk2
#' @importFrom dplyr left_join bind_cols
#' @importFrom glue glue glue_collapse
#' @importFrom tidyselect everything
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

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

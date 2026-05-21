#' Deprecated - Split xpt file output
#'
#' @description
#' \strong{Deprecated.}
#'
#' This function is *deprecated*. Please use the argument
#'  `max_gb_size` in the function xportr_write()` instead.
#'
#' Per the FDA Study Data Technical Conformance
#' Guide(https://www.fda.gov/media/88173/download) section 3.3.2, dataset files
#' sizes shouldn't exceed 5 GB. If datasets are large enough, they should be
#' split based on a variable. For example, laboratory readings in `ADLB` can be
#' split by `LBCAT` to split up hematology and chemistry data.
#'
#' This function will tell `xportr_write()` to split the data frame based on the
#' variable passed in `split_by`. When written, the file name will be prepended
#' with a number for uniqueness. These files should be noted in the Reviewer Guides per
#' CDISC guidance to note how you split your files.
#'
#' @inheritParams xportr_length
#' @param split_by A quoted variable that will be passed to `base::split()`.
#'
#' @return A data frame with an additional attribute added so `xportr_write()`
#'   knows how to split the data frame.
#'
#'
#'
#' @export
#'
#' @examples
#'
#' adlb <- data.frame(
#'   USUBJID = c(1001, 1002, 1003),
#'   LBCAT = c("HEMATOLOGY", "HEMATOLOGY", "CHEMISTRY")
#' )
#'
#' adlb <- xportr_split(adlb, "LBCAT")
xportr_split <- function(.df, split_by = NULL) {
  .Deprecated(
    new = "xportr_write",
    msg = paste0(
      "'xportr_split()' was deprecated in xportr 0.4.1.\n",
      "Please use the argument `max_gb_size` in `xportr_write()` instead."
    )
  )
  .df <- group_data_check(.df)
  attr(.df, "_xportr.split_by_") <- split_by
  .df
}

#' @rdname xportr_split-deprecated



#' Title
#'
#' @inheritParams xportr_length
#' @param split_by
#'
#' @return A data frame with an additonal attribnute added so `xportr_write`
#'   knows how to split the data frame.
#'
#'
#' @export
#'
#' @examples
xportr_split <- function(.df, split_by = NULL) {

  attr(.df, "_xportr.split_by_") <- split_by

}

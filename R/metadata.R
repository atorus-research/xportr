#' Set variable specifications and domain
#'
#' Sets metadata and/or domain for a dataset in a way that can be accessed by
#' other xportr functions. If used at the start of an xportr pipeline, it
#' removes the need to set metadata and domain at each step individually. For
#' details on the format of the metadata, see the 'Metadata' section for each
#' function in question.
#'
#' @inheritParams xportr_length
#'
#' @return `.df` dataset with metadata and domain attributes set
#' @export
#'
#' @rdname metadata
#'
#' @examples
#'
#' metadata <- data.frame(
#'   dataset = "test",
#'   variable = c("Subj", "Param", "Val", "NotUsed"),
#'   type = c("numeric", "character", "numeric", "character"),
#'   format = NA,
#'   order = c(1, 3, 4, 2)
#' )
#'
#' adlb <- data.frame(
#'   Subj = as.character(123, 456, 789),
#'   Different = c("a", "b", "c"),
#'   Val = c("1", "2", "3"),
#'   Param = c("param1", "param2", "param3")
#' )
#'
#' xportr_metadata(adlb, metadata, "test")
#'
#' if (rlang::is_installed("magrittr")) {
#'   library(magrittr)
#'
#'   adlb %>%
#'     xportr_metadata(metadata, "test") %>%
#'     xportr_type() %>%
#'     xportr_order()
#' }
xportr_metadata <- function(.df, metadata = NULL, domain = NULL) {
  assert_data_frame(.df)
  if (is.null(metadata) && is.null(domain)) {
    stop("Assertion failed on `metadata` and `domain`: Must provide either `metadata` or `domain` argument")
  }
  assert_metadata(metadata, include_fun_message = FALSE, null.ok = TRUE)
  assert_string(domain, null.ok = TRUE)
  
  ## Common section to detect domain from argument or attribute

  if (!is.null(domain)) .df <- xportr_domain_name(.df, domain)

  structure(.df, "_xportr.df_metadata_" = metadata)
}

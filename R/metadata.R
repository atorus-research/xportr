#' Set variable specifications and domain
#'
#' Sets metadata for a dataset in a way that can be accessed by other xportr
#' functions. If used at the start of an xportr pipeline, it removes the need to
#' set metadata and domain at each step individually. For details on the format
#' of the metadata, see the 'Metadata' section for each function in question.
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
#'     xportr_domain_name("adlb") %>%
#'     xportr_metadata(metadata, "test") %>%
#'     xportr_type() %>%
#'     xportr_order()
#' }
xportr_metadata <- function(.df, metadata, domain = NULL) {
  assert_data_frame(.df)
  assert(
    combine = "or",
    check_r6(metadata, "Metacore", null.ok = TRUE),
    check_data_frame(metadata, null.ok = TRUE)
  )
  assert_string(domain, null.ok = TRUE)

  ## Common section to detect domain from argument or pipes

  domain <- get_domain(.df, domain)
  if (!is.null(domain)) attr(.df, "_xportr.df_arg_") <- domain

  ## End of common section

  structure(.df, "_xportr.df_metadata_" = metadata)
}


#' Update Metadata Domain Name
#'
#' @inheritParams xportr_length
#'
#' @return `.df` dataset with domain argument set
#' @export
#'
#' @rdname metadata
xportr_domain_name <- function(.df, domain) {

  attr(.df, "_xportr.df_arg_") <- domain

  .df
}

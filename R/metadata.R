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
xportr_metadata <- function(.df, metadata, domain = NULL) {
  ## Common section to detect domain from argument or pipes

  df_arg <- tryCatch(as_name(enexpr(.df)), error = function(err) NULL)
  domain <- get_domain(.df, df_arg, domain)
  if (!is.null(domain)) attr(.df, "_xportr.df_arg_") <- domain

  ## End of common section

  structure(.df, `_xportr.df_metadata_` = metadata)
}

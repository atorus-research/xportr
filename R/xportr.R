#' Wrapper to apply all core xportr functions and write xpt
#'
#' @param .df A data frame of CDISC standard.
#' @param var_metadata A data frame containing variable level metadata
#' @param domain Appropriate CDISC dataset name, e.g. ADAE, DM. Used to subset
#'   the metadata object.
#' @param verbose The action this function takes when an action is taken on the
#'   dataset or function validation finds an issue. See 'Messaging' section for
#'   details. Options are 'stop', 'warn', 'message', and 'none'
#' @param df_metadata A data frame containing dataset level metadata.
#' @param path Path where transport file will be written. File name sans will be
#'   used as `xpt` name.
#' @param strict_checks If TRUE, xpt validation will report errors and not write
#'   out the dataset. If FALSE, xpt validation will report warnings and continue
#'   with writing out the dataset. Defaults to FALSE
#'
#' @return Returns the input dataframe invisibly
#' @export
#'
#' @examplesIf requireNamespace("magrittr")
#' data("adsl_xportr", "dataset_spec", "var_spec")
#' adsl <- adsl_xportr
#'
#' library(magrittr)
#' test_dir <- tempdir()
#'
#' pipeline_path <- file.path(test_dir, "adslpipe.xpt")
#' xportr_path <- file.path(test_dir, "adslxptr.xpt")
#'
#' dataset_spec_low <- setNames(dataset_spec, tolower(names(dataset_spec)))
#' names(dataset_spec_low)[[2]] <- "label"
#'
#' var_spec_low <- setNames(var_spec, tolower(names(var_spec)))
#' names(var_spec_low)[[5]] <- "type"
#'
#' adsl %>%
#'   xportr_metadata(var_spec_low, "ADSL", verbose = "none") %>%
#'   xportr_type() %>%
#'   xportr_length() %>%
#'   xportr_label() %>%
#'   xportr_order() %>%
#'   xportr_format() %>%
#'   xportr_df_label(dataset_spec_low) %>%
#'   xportr_write(pipeline_path)
#'
#' # `xportr()` can be used to apply a whole pipeline at once
#' xportr(
#'   adsl,
#'   var_metadata = var_spec_low,
#'   df_metadata = dataset_spec_low,
#'   domain = "ADSL",
#'   verbose = "none",
#'   path = xportr_path
#' )
xportr <- function(.df,
                   var_metadata = NULL,
                   df_metadata = NULL,
                   domain = NULL,
                   verbose = NULL,
                   path,
                   strict_checks = FALSE) {
  .df %>%
    xportr_metadata(var_metadata, domain = domain, verbose = verbose) %>%
    xportr_type() %>%
    xportr_length() %>%
    xportr_label() %>%
    xportr_order() %>%
    xportr_format() %>%
    xportr_df_label(df_metadata) %>%
    xportr_write(path)
}

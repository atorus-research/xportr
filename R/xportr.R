#' Wrapper to apply all core xportr functions and write xpt
#'
#' @param .df A data frame of CDISC standard.
#' @param var_metadata A data frame containing variable level metadata
#' @param domain Appropriate CDSIC dataset name, e.g. ADAE, DM. Used to subset
#'   the metadata object. If none is passed, then name of the dataset passed as
#'   .df will be used.
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
#' @examples
#'
#' has_pkgs <- requireNamespace("admiral", quietly = TRUE) &&
#'   requireNamespace("dplyr", quietly = TRUE) &&
#'   requireNamespace("readxl", quietly = TRUE) &&
#'   requireNamespace("rlang", quietly = TRUE)
#'
#' if (has_pkgs) {
#'   adsl <- admiral::admiral_adsl
#'
#'   spec_path <- system.file(paste0("specs/", "ADaM_admiral_spec.xlsx"), package = "xportr")
#'
#'   var_spec <- readxl::read_xlsx(spec_path, sheet = "Variables") %>%
#'     dplyr::rename(type = "Data Type") %>%
#'     rlang::set_names(tolower)
#'   dataset_spec <- readxl::read_xlsx(spec_path, sheet = "Datasets") %>%
#'     dplyr::rename(label = "Description") %>%
#'     rlang::set_names(tolower)
#'
#'   adsl %>%
#'     xportr_metadata(var_spec, "ADSL", verbose = "warn") %>%
#'     xportr_type() %>%
#'     xportr_length() %>%
#'     xportr_label() %>%
#'     xportr_order() %>%
#'     xportr_format() %>%
#'     xportr_df_label(dataset_spec) %>%
#'     xportr_write("adsl.xpt")
#'
#'   # `xportr()` can be used to apply a whole pipeline at once
#'   xportr(adsl,
#'     var_metadata = var_spec,
#'     df_metadata = dataset_spec,
#'     domain = "ADSL",
#'     verbose = "warn",
#'     path = "adsl.xpt"
#'   )
#' }
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

#' Assign Dataset Label
#'
#' Assigns dataset label from a dataset level metadata to a given data frame.
#' This is stored in the 'label' attribute of the dataframe.
#'
#' @param metadata A data frame containing dataset. See 'Metadata' section for
#'   details.
#' @inheritParams xportr_length
#'
#' @return Data frame with label attributes.
#'
#' @section Metadata: The argument passed in the 'metadata' argument can either
#'   be a metacore object, or a data.frame containing the data listed below. If
#'   metacore is used, no changes to options are required.
#'
#'   For data.frame 'metadata' arguments two columns must be present:
#'
#'   1) Domain Name - passed as the 'xportr.df_domain_name' option. Default:
#'   "dataset". This is the column subset by the 'domain' argument in the
#'   function.
#'
#'   2) Label Name - passed as the 'xportr.df_label' option. Default:
#'   "label". Character values to update the 'label' attribute of the
#'   dataframe This is passed to `haven::write_xpt` to note the label.
#'
#' @export
#'
#' @examples
#' adsl <- data.frame(
#'   USUBJID = c(1001, 1002, 1003),
#'   SITEID = c(001, 002, 003),
#'   AGE = c(63, 35, 27),
#'   SEX = c("M", "F", "M")
#' )
#'
#' metadata <- data.frame(
#'   dataset = c("adsl", "adae"),
#'   label = c("Subject-Level Analysis", "Adverse Events Analysis")
#' )
#'
#' adsl <- xportr_df_label(adsl, metadata, domain = "adsl")
xportr_df_label <- function(.df,
                            metadata = NULL,
                            domain = NULL,
                            metacore = deprecated()) {
  if (!missing(metacore)) {
    lifecycle::deprecate_stop(
      when = "0.3.1.9005",
      what = "xportr_df_label(metacore = )",
      with = "xportr_df_label(metadata = )"
    )
  }

  ## Common section to detect default arguments

  domain <- domain %||% attr(.df, "_xportr.df_arg_")
  if (!is.null(domain)) attr(.df, "_xportr.df_arg_") <- domain

  metadata <- metadata %||% attr(.df, "_xportr.df_metadata_")

  ## End of common section

  assert_data_frame(.df)
  assert_string(domain, null.ok = TRUE)
  assert_metadata(metadata)

  domain_name <- getOption("xportr.df_domain_name")
  label_name <- getOption("xportr.df_label")

  if (inherits(metadata, "Metacore")) metadata <- metadata$ds_spec

  label <- metadata %>%
    filter(!!sym(domain_name) == .env$domain) %>%
    select(!!sym(label_name)) %>%
    # If a dataframe is used this will also be a dataframe, change to character.
    as.character()

  if (!test_string(label, max.chars = 40)) {
    abort("Length of dataset label must be 40 characters or less.")
  }

  if (stringr::str_detect(label, "[^[:ascii:]]")) {
    abort("`label` cannot contain any non-ASCII, symbol or special characters.")
  }

  attr(.df, "label") <- label

  .df
}

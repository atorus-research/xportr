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
#'   "format". Character values to update the 'format.sas' attribute of the
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
#' adsl <- xportr_df_label(adsl, metadata)
xportr_df_label <- function(.df,
                            metadata = NULL,
                            domain = NULL,
                            metacore = deprecated()) {
  if (!missing(metacore)) {
    lifecycle::deprecate_warn(
      when = "0.3.0",
      what = "xportr_df_label(metacore = )",
      with = "xportr_df_label(metadata = )"
    )
    metadata <- metacore
  }
  domain_name <- getOption("xportr.df_domain_name")
  label_name <- getOption("xportr.df_label")

  ## Common section to detect domain from argument or pipes

  df_arg <- tryCatch(as_name(enexpr(.df)), error = function(err) NULL)
  domain <- get_domain(.df, df_arg, domain)
  if (!is.null(domain)) attr(.df, "_xportr.df_arg_") <- domain

  ## End of common section

  ## Pull out correct metadata
  metadata <- metadata %||%
    attr(.df, "_xportr.df_metadata_") %||%
    rlang::abort("Metadata must be set with `metadata` or `xportr_metadata()`")

  if (inherits(metadata, "Metacore")) {
    metadata <- metadata$ds_spec
  }

  label <- metadata %>%
    filter(!!sym(domain_name) == domain) %>%
    select(!!sym(label_name)) %>%
    # If a dataframe is used this will also be a dataframe, change to character.
    as.character()

  label_len <- nchar(label)

  if (label_len > 40) {
    abort("Length of dataset label must be 40 characters or less.")
  }

  attr(.df, "label") <- label

  .df
}

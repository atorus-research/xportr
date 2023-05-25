#' Assign Dataset Label
#'
#' Assigns dataset label from a dataset level metadata to a given data frame.
#'
#' @param .df A data frame of CDISC standard.
#' @param metadata A data frame containing dataset level metadata.
#' @param domain A character value to subset the `.df`. If `NULL`(default), uses
#'   `.df` value as a subset condition.
#' @param metacore `r lifecycle::badge("deprecated")` Previously used to pass metadata now renamed with `metadata`
#'
#' @return Data frame with label attributes.
#' @family metadata functions
#' @seealso [xportr_label()], [xportr_format()] and [xportr_length()]
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
xportr_df_label <- function(.df, metadata, domain = NULL, metacore = deprecated()) {
  if (!missing(metacore)) {
    lifecycle::deprecate_warn(
      when = "0.3.0",
      what = "xportr_format(metacore = )",
      with = "xportr_format(metadata = )"
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

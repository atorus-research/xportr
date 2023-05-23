#' Assign Variable Label
#'
#' Assigns variable label from a variable level metadata to a given data frame.
#'
#' @param .df A data frame of CDISC standard.
#' @param metadata A data frame containing variable level metadata.
#' @param domain A character value to subset the `.df`. If `NULL`(default), uses
#'   `.df` value as a subset condition.
#' @param verbose The action the function takes when a variable length isn't
#'   Found. Options are 'stop', 'warn', 'message', and 'none'
#' @param metacore `r lifecycle::badge("deprecated")` previously used to pass metadata now renamed with `metadata`
#'
#' @return Data frame with label attributes for each variable.
#' @family metadata functions
#' @seealso [xportr_df_label()], [xportr_format()] and [xportr_length()]
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
#'   dataset = "adsl",
#'   variable = c("USUBJID", "SITEID", "AGE", "SEX"),
#'   label = c("Unique Subject Identifier", "Study Site Identifier", "Age", "Sex")
#' )
#'
#' adsl <- xportr_label(adsl, metadata)
xportr_label <- function(
    .df,
    metadata,
    domain = NULL,
    verbose = getOption("xportr.length_verbose", "none"),
    metacore = deprecated()) {
  if (!missing(metacore)) {
    lifecycle::deprecate_warn(
      when = "0.0.3",
      what = "xportr_format(metacore = )",
      with = "xportr_format(metadata = )"
    )
    metadata <- metacore
  }
  domain_name <- getOption("xportr.domain_name")
  variable_name <- getOption("xportr.variable_name")
  variable_label <- getOption("xportr.label")

  ## Common section to detect domain from argument or pipes

  df_arg <- tryCatch(as_name(enexpr(.df)), error = function(err) NULL)
  domain <- get_domain(.df, df_arg, domain)
  if (!is.null(domain)) attr(.df, "_xportr.df_arg_") <- domain

  ## End of common section

  if (inherits(metadata, "Metacore")) {
    metadata <- metadata$var_spec
  }

  if (domain_name %in% names(metadata)) {
    metadata <- metadata %>%
      dplyr::filter(!!sym(domain_name) == domain)
  } else {
    metadata <- metadata
  }


  # Check any variables missed in metadata but present in input data ---
  miss_vars <- setdiff(names(.df), metadata[[variable_name]])

  label_log(miss_vars, verbose)

  label <- metadata[[variable_label]]
  names(label) <- metadata[[variable_name]]

  # Check any variable label have more than 40 characters ---
  label_len <- lapply(label, nchar)
  err_len <- which(label_len > 40) %>% names()

  if (length(err_len) > 0) {
    warn(
      c("Length of variable label must be 40 characters or less.",
        x = glue("Problem with {encode_vars(err_len)}.")
      )
    )
  }

  for (i in names(.df)) {
    if (i %in% miss_vars) {
      attr(.df[[i]], "label") <- ""
    } else {
      attr(.df[[i]], "label") <- label[[i]]
    }
  }

  .df
}

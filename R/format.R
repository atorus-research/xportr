#' Assign SAS Format
#'
#' Assigns a SAS format from a variable level metadata to a given data frame. If
#' no format is found for a given variable, it is set as an empty character
#' vector. This is stored in the format.sas attribute.
#'
#' @inheritParams xportr_length
#'
#' @return Data frame with `SASformat` attributes for each variable.
#'
#' @section Metadata: The argument passed in the 'metadata' argument can either
#'   be a metacore object, or a data.frame containing the data listed below. If
#'   metacore is used, no changes to options are required.
#'
#'   For data.frame 'metadata' arguments three columns must be present:
#'
#'   1) Domain Name - passed as the 'xportr.domain_name' option. Default:
#'   "dataset". This is the column subset by the 'domain' argument in the
#'   function.
#'
#'   2) Format Name - passed as the 'xportr.format_name' option.
#'   Default: "format". Character values to update the 'format.sas' attribute of
#'   the column. This is passed to `haven::write` to note the format.
#'
#'   3) Variable Name - passed as the 'xportr.variable_name' option. Default:
#'   "variable". This is used to match columns in '.df' argument and the
#'   metadata.
#'
#' @export
#'
#' @examples
#' adsl <- data.frame(
#'   USUBJID = c(1001, 1002, 1003),
#'   BRTHDT = c(1, 1, 2)
#' )
#'
#' metadata <- data.frame(
#'   dataset = c("adsl", "adsl"),
#'   variable = c("USUBJID", "BRTHDT"),
#'   format = c(NA, "DATE9.")
#' )
#'
#' adsl <- xportr_format(adsl, metadata)
xportr_format <- function(.df,
                          metadata = NULL,
                          domain = NULL,
                          metacore = deprecated()) {
  if (!missing(metacore)) {
    lifecycle::deprecate_warn(
      when = "0.3.0",
      what = "xportr_format(metacore = )",
      with = "xportr_format(metadata = )"
    )
    metadata <- metacore
  }
  domain_name <- getOption("xportr.domain_name")
  format_name <- getOption("xportr.format_name")
  variable_name <- getOption("xportr.variable_name")

  ## Common section to detect domain from argument or pipes

  df_arg <- tryCatch(as_name(enexpr(.df)), error = function(err) NULL)
  domain <- get_domain(.df, df_arg, domain)
  if (!is.null(domain)) attr(.df, "_xportr.df_arg_") <- domain

  ## End of common section

  metadata <- metadata %||%
    attr(.df, "_xportr.df_metadata_") %||%
    rlang::abort("Metadata must be set with `metadata` or `xportr_metadata()`")

  if (inherits(metadata, "Metacore")) {
    metadata <- metadata$var_spec
  }

  if (domain_name %in% names(metadata)) {
    metadata <- metadata %>%
      dplyr::filter(!!sym(domain_name) == domain & !is.na(!!sym(format_name)))
  } else {
    # Common check for multiple variables name
    check_multiple_var_specs(metadata, variable_name)
  }

  filtered_metadata <- metadata %>%
    filter(!!sym(variable_name) %in% names(.df))

  format <- filtered_metadata %>%
    select(!!sym(format_name)) %>%
    unlist() %>%
    toupper()

  names(format) <- filtered_metadata[[variable_name]]

  for (i in seq_len(ncol(.df))) {
    format_sas <- purrr::pluck(format, colnames(.df)[i])
    if (is.na(format_sas) || is.null(format_sas)) {
      format_sas <- ""
    }
    attr(.df[[i]], "format.sas") <- format_sas
  }

  .df
}

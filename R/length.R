#' Assign SAS Length
#'
#' Assigns SAS length from a variable level metadata to a given data frame.
#'
#' @param .df A data frame of CDISC standard.
#' @param metadata A data frame containing variable level metadata.
#' @param domain A character value to subset the `.df`. If `NULL`(default), uses
#'   `.df` value as a subset condition.
#' @param verbose The action the function takes when a length isn't found in
#'   metadata. Options are 'stop', 'warn', 'message', and 'none'
#' @param metacore `r lifecycle::badge("deprecated")` Previously used to pass metadata now renamed with `metadata`
#'
#' @return Data frame with `SASlength` attributes for each variable.
#' @family metadata functions
#' @seealso [xportr_label()], [xportr_df_label()] and [xportr_format()]
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
#'   length = c(10, 8)
#' )
#'
#' adsl <- xportr_length(adsl, metadata)
xportr_length <- function(.df,
                          metadata = NULL,
                          domain = NULL,
                          verbose = getOption("xportr.length_verbose", "none"),
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
  variable_length <- getOption("xportr.length")
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
      filter(!!sym(domain_name) == domain)
  } else {
    metadata <- metadata
  }


  # Check any variables missed in metadata but present in input data ---
  miss_vars <- setdiff(names(.df), metadata[[variable_name]])

  length_log(miss_vars, verbose)

  length <- metadata[[variable_length]]
  names(length) <- metadata[[variable_name]]

  for (i in names(.df)) {
    if (i %in% miss_vars) {
      attr(.df[[i]], "width") <- impute_length(.df[[i]])
    } else {
      attr(.df[[i]], "width") <- length[[i]]
    }
  }

  .df
}

impute_length <- function(col) {
  characterTypes <- getOption("xportr.character_types")
  # first_class will collapse to character if it is the option
  if (first_class(col) %in% "character") {
    200
  } else {
    8
  }
}

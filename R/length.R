#' Assign SAS Length
#'
#' Assigns SAS length from a metadata object to a given data frame. If a
#' length isn't present for a variable the length value is set to 200 for
#' character columns, and 8 for non-character columns. This value is stored in
#' the 'width' attribute of the column.
#'
#' @param .df A data frame of CDISC standard.
#' @param metadata A data frame containing variable level metadata. See
#'   'Metadata' section for details.
#' @param domain Appropriate CDSIC dataset name, e.g. ADAE, DM. Used to subset
#'   the metadata object. If none is passed, then name of the dataset passed as
#'   .df will be used.
#' @param verbose The action this function takes when an action is taken on the
#'   dataset or function validation finds an issue. See 'Messaging' section for
#'   details. Options are 'stop', 'warn', 'message', and 'none'
#' @param metacore `r lifecycle::badge("deprecated")` Previously used to pass
#'   metadata now renamed with `metadata`
#'
#' @section Messaging: `length_log` is the primary messaging tool for
#'   `xportr_length`. If there are any columns present in the '.df' that are not
#'   noted in the metadata, they cannot be assigned a length and a message will
#'   be generated noting the number or variables that have not been assigned a
#'   length.
#'
#'   If variables were not found in the metadata and the value passed to the
#'   'verbose' argument is 'stop', 'warn', or 'message', a message will be
#'   generated detailing the variables that were missing in the metadata.
#'
#' @section Metadata: The argument passed in the 'metadata' argument can either
#'   be a `{metacore}` object, or a data.frame containing the data listed below. If
#'   metacore is used, no changes to options are required.
#'
#'   For data.frame 'metadata' arguments three columns must be present:
#'
#'   1) Domain Name - passed as the 'xportr.domain_name' option. Default:
#'   "dataset". This is the column subset by the 'domain' argument in the
#'   function.
#'
#'   2) Variable Name - passed as the 'xportr.variable_name' option.
#'   Default: "variable". This is used to match columns in '.df' argument and
#'   the metadata.
#'
#'   3) Variable Label - passed as the 'xportr.length' option.
#'   Default: "length". These numeric values to update the 'width' attribute of
#'   the column. This is passed to `haven::write` to note the variable length.
#'
#'
#' @return Data frame with `SASlength` attributes for each variable.
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
      what = "xportr_length(metacore = )",
      with = "xportr_length(metadata = )"
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
    # Common check for multiple variables name
    check_multiple_var_specs(metadata, variable_name)
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

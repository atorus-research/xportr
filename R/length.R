#' Assign SAS Length
#'
#' Assigns the SAS length to a specified data frame, either from a metadata object
#' or based on the calculated maximum data length. If a length isn't present for
#' a variable the length value is set to maximum data length for character columns, and 8
#' for non-character columns. This value is stored in the 'width' attribute of the column.
#'
#' @inheritParams xportr
#' @param metadata A data frame containing variable level metadata. See
#'   'Metadata' section for details.
#' @param length_source Choose the assigned length from either metadata or data.
#'
#'   If `"metadata"` is specified, the assigned length is from the metadata length.
#'   If `"data"` is specified, the assigned length is determined by the calculated maximum data length.
#'
#'   *Permitted Values*: `"metadata"`, `"data"`
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
#' @return Data frame with SAS default length attributes for each variable.
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
#' adsl <- xportr_length(adsl, metadata, domain = "adsl", length_source = "metadata")
xportr_length <- function(.df,
                          metadata = NULL,
                          domain = NULL,
                          verbose = NULL,
                          length_source = c("metadata", "data"),
                          metacore = deprecated()) {
  length_source <- match.arg(length_source)
  if (!missing(metacore)) {
    lifecycle::deprecate_stop(
      when = "0.3.1.9005",
      what = "xportr_length(metacore = )",
      with = "xportr_length(metadata = )"
    )
  }

  ## Common section to detect default arguments

  domain <- domain %||% attr(.df, "_xportr.df_arg_")
  if (!is.null(domain)) attr(.df, "_xportr.df_arg_") <- domain

  metadata <- metadata %||% attr(.df, "_xportr.df_metadata_")

  # Verbose should use an explicit verbose option first, then the value set in
  # metadata, and finally fall back to the option value
  verbose <- verbose %||%
    attr(.df, "_xportr.df_verbose_") %||%
    getOption("xportr.length_verbose", "none")

  ## End of common section

  assert_data_frame(.df)
  assert_string(domain, null.ok = TRUE)
  assert_metadata(metadata)
  assert_choice(verbose, choices = .internal_verbose_choices)

  domain_name <- getOption("xportr.domain_name")
  variable_length <- getOption("xportr.length")
  variable_name <- getOption("xportr.variable_name")

  if (inherits(metadata, "Metacore")) metadata <- metadata$var_spec

  if (domain_name %in% names(metadata) && !is.null(domain)) {
    metadata <- metadata %>%
      filter(!!sym(domain_name) == .env$domain)
  } else {
    # Common check for multiple variables name
    check_multiple_var_specs(metadata, variable_name)
  }

  # Get max length for missing length and when length_source == "data"
  var_length_max <- variable_max_length(.df)

  length_data <- var_length_max[[variable_length]]
  names(length_data) <- var_length_max[[variable_name]]

  # Check any variables missed in metadata but present in input data ---
  miss_vars <- setdiff(names(.df), metadata[[variable_name]])

  miss_length <- character(0L)
  width_attr <- if (identical(length_source, "metadata")) {
    length_metadata <- metadata[[variable_length]]
    names(length_metadata) <- metadata[[variable_name]]

    # Check any variables with missing length in metadata
    miss_length <- names(length_metadata[is.na(length_metadata)])

    # Build `width` attribute
    vapply(
      names(.df),
      function(i) {
        if (i %in% miss_vars || is.na(length_metadata[[i]])) {
          as.numeric(length_data[[i]])
        } else {
          as.numeric(length_metadata[[i]])
        }
      },
      numeric(1L)
    )
  } else if (identical(length_source, "data")) {
    length_msg <- left_join(var_length_max, metadata[, c(variable_name, variable_length)], by = variable_name)
    length_msg <- length_msg %>%
      mutate(
        length_df = as.numeric(length_msg[[paste0(variable_length, ".x")]]),
        length_meta = as.numeric(length_msg[[paste0(variable_length, ".y")]])
      ) %>%
      filter(.data$length_df < .data$length_meta) %>%
      select(any_of(c(variable_name, "length_df", "length_meta")))

    max_length_msg(length_msg, verbose)

    # Build `width` attribute
    length_data[names(.df)]
  }

  for (i in names(.df)) {
    attr(.df[[i]], "width") <- width_attr[[i]]
  }

  # Message for missing var and missing length
  length_log(miss_vars, miss_length, verbose)

  .df
}

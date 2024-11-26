#' Assign Variable Label
#'
#' Assigns variable label from a variable level metadata to a given data frame.
#' This function will give detect if a label is greater than
#' 40 characters which isn't allowed in XPT v5. If labels aren't present for the
#' variable it will be assigned an empty character value. Labels are stored in
#' the 'label' attribute of the column.
#'
#' @inheritParams xportr_length
#'
#' @section Messaging: `label_log()` is the primary messaging tool for
#'   `xportr_label()`. If there are any columns present in the '.df' that are not
#'   noted in the metadata, they cannot be assigned a label and a message will
#'   be generated noting the number or variables that have not been assigned a
#'   label.
#'
#'   If variables were not found in the metadata and the value passed to the
#'   'verbose' argument is 'stop', 'warn', or 'message', a message will be
#'   generated detailing the variables that were missing in metadata.
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
#'   2) Variable Name - passed as the 'xportr.variable_name' option.
#'   Default: "variable". This is used to match columns in '.df' argument and
#'   the metadata.
#'
#'   3) Variable Label - passed as the 'xportr.label' option.
#'   Default: "label". These character values to update the 'label' attribute of
#'   the column. This is passed to `haven::write` to note the label.
#'
#'
#' @return Data frame with label attributes for each variable.
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
#'   dataset = "adsl",
#'   variable = c("USUBJID", "SITEID", "AGE", "SEX"),
#'   label = c("Unique Subject Identifier", "Study Site Identifier", "Age", "Sex")
#' )
#'
#' adsl <- xportr_label(adsl, metadata, domain = "adsl")
xportr_label <- function(.df,
                         metadata = NULL,
                         domain = NULL,
                         verbose = NULL,
                         metacore = deprecated()) {
  if (!missing(metacore)) {
    lifecycle::deprecate_stop(
      when = "0.3.1.9005",
      what = "xportr_label(metacore = )",
      with = "xportr_label(metadata = )"
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
    getOption("xportr.label_verbose", "none")

  ## End of common section

  assert_data_frame(.df)
  assert_string(domain, null.ok = TRUE)
  assert_metadata(metadata)
  assert_choice(verbose, choices = .internal_verbose_choices)

  domain_name <- getOption("xportr.domain_name")
  variable_name <- getOption("xportr.variable_name")
  variable_label <- getOption("xportr.label")

  if (inherits(metadata, "Metacore")) metadata <- metadata$var_spec

  if (domain_name %in% names(metadata) && !is.null(domain)) {
    # If 'domain' passed by user isn't found in metadata, return error
    if (!domain %in% metadata[[domain_name]]) log_no_domain(domain, domain_name, verbose)

    metadata <- metadata %>%
      dplyr::filter(!!sym(domain_name) == .env$domain)
  } else {
    # Common check for multiple variables name
    check_multiple_var_specs(metadata, variable_name)
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
    attr(.df[[i]], "label") <- if (i %in% miss_vars) {
      ""
    } else {
      label[[i]]
    }
  }

  .df
}

#' Order variables of a dataset according to Spec
#'
#' The `dplyr::arrange()` function is used to order the columns of the dataframe.
#' Any variables that are missing an order value are appended to the end of the dataframe
#' after all of the variables that have an order.
#'
#' @inheritParams xportr_length
#'
#' @export
#'
#' @section Messaging: There are three primary messages output by `xportr_order()`.
#'
#'   The first identifies the "moved" variables. These are the variables that were
#'   either not found in the metadata file or had missing order values, and therefore
#'   moved to the end of the dataset. A message will be generated noting the number,
#'   if any, of variables that were moved to the end of the dataset.
#'
#'   The second message identifies the "reordered" variables. These are the variables
#'   that were in the dataset, but not in the correct order. A message will be generated
#'   noting the number, if any, of variables that have been reordered.
#'
#'   The third message identifies the "skipped" metadata variables. These are the
#'   metadata variables missing from the dataset and therefore skipped from processing.
#'   A message will be generated noting the number, if any, of metadata variables
#'   that have been skipped.
#'
#'   In all three cases, if the value passed to the `verbose` argument is `stop`,
#'   `warn`, or `message`, a complete list of the affected variables will be provided.
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
#'   3) Variable Order - passed as the 'xportr.order_name' option.
#'   Default: "order". These values used to arrange the order of the variables.
#'   If the values of order metadata are not numeric, they will be coerced to
#'   prevent alphabetical sorting of numeric values.
#'
#' @return Dataframe that has been re-ordered according to spec
#'
#' @examples
#' adsl <- data.frame(
#'   BRTHDT = c(1, 1, 2),
#'   STUDYID = c("mid987650", "mid987650", "mid987650"),
#'   TRT01A = c("Active", "Active", "Placebo"),
#'   USUBJID = c(1001, 1002, 1003)
#' )
#'
#' metadata <- data.frame(
#'   dataset = c("adsl", "adsl", "adsl", "adsl"),
#'   variable = c("STUDYID", "USUBJID", "TRT01A", "BRTHDT"),
#'   order = 1:4
#' )
#'
#' adsl <- xportr_order(adsl, metadata, domain = "adsl")
xportr_order <- function(.df,
                         metadata = NULL,
                         domain = NULL,
                         verbose = NULL) {
  ## Common section to detect default arguments

  domain <- domain %||% attr(.df, "_xportr.df_arg_")
  if (!is.null(domain)) attr(.df, "_xportr.df_arg_") <- domain

  metadata <- metadata %||% attr(.df, "_xportr.df_metadata_")

  # Verbose should use an explicit verbose option first, then the value set in
  # metadata, and finally fall back to the option value
  verbose <- verbose %||%
    attr(.df, "_xportr.df_verbose_") %||%
    getOption("xportr.order_verbose", "none")

  ## End of common section
  assert_data_frame(.df)
  assert_string(domain, null.ok = TRUE)
  assert_metadata(metadata)
  assert_choice(verbose, choices = .internal_verbose_choices)
  .df <- group_data_check(.df, verbose = verbose)

  domain_name <- getOption("xportr.domain_name")
  order_name <- getOption("xportr.order_name")
  variable_name <- getOption("xportr.variable_name")

  if (inherits(metadata, "Metacore")) metadata <- metadata$ds_vars

  if (domain_name %in% names(metadata) && !is.null(domain)) {
    # If 'domain' passed by user isn't found in metadata, return error
    if (!domain %in% metadata[[domain_name]]) log_no_domain(domain, domain_name, verbose)

    metadata <- metadata %>%
      filter(!!sym(domain_name) == .env$domain)
  } else {
    # Common check for multiple variables name
    check_multiple_var_specs(metadata, variable_name)
  }

  # Variables in metadata but not in dataset
  miss_meta_vars <- setdiff(metadata[[variable_name]], names(.df))

  # In the metadata, only keep entries(rows) with non-NA order values
  metadata <- metadata %>%
    filter(!is.na(!!sym(order_name)))

  # Grabs vars from Spec and inputted dataset
  vars_in_spec_ds <- metadata[, c(variable_name, order_name)] %>%
    mutate(!!sym(order_name) := as.numeric(!!sym(order_name))) %>%
    arrange(!!sym(order_name)) %>%
    extract2(variable_name)

  vars_in_spec_ds <- vars_in_spec_ds[!is.na(vars_in_spec_ds)]
  # Grabs all variables from Spec file and orders accordingly
  ord_vars <- .df %>%
    select(any_of(vars_in_spec_ds))

  # Variables not in Spec file - will be moved to the end
  drop_vars <- .df %>%
    select(!any_of(vars_in_spec_ds))

  df_re_ord <- bind_cols(ord_vars, drop_vars)

  # Used in warning message for how many vars have been moved
  reorder_vars <- names(df_re_ord)[names(df_re_ord) != names(.df)]

  # Function is located in messages.R
  var_ord_msg(reorder_vars, names(drop_vars), verbose)

  # Message for missing metadata variables
  metadata_vars_log(miss_meta_vars, verbose)

  df_re_ord
}

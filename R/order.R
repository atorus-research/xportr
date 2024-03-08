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
#' @section Messaging: `var_ord_msg()` is the primary messaging tool for
#'   `xportr_order()`. There are two primary messages that are output from
#'   `var_ord_msg()`. The first is the "moved" variables. These are the variables
#'   that were not found in the metadata file and moved to the end of the
#'   dataset. A message will be generated noting the number, if any, of
#'   variables that were moved to the end of the dataset. If any variables were
#'   moved, and the 'verbose' argument is 'stop', 'warn', or 'message', a
#'   message will be generated detailing the variables that were moved.
#'
#'   The second primary message is the number of variables that were in the
#'   dataset, but not in the correct order. A message will be generated noting
#'   the number, if any, of variables that have been reordered. If any variables
#'   were reordered, and the 'verbose' argument is 'stop', 'warn', or 'message',
#'   a message will be generated detailing the variables that were reordered.
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
                         verbose = NULL,
                         metacore = deprecated()) {
  if (!missing(metacore)) {
    lifecycle::deprecate_stop(
      when = "0.3.1.9005",
      what = "xportr_order(metacore = )",
      with = "xportr_order(metadata = )"
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
    getOption("xportr.order_verbose", "none")

  ## End of common section

  assert_data_frame(.df)
  assert_string(domain, null.ok = TRUE)
  assert_metadata(metadata)
  assert_choice(verbose, choices = .internal_verbose_choices)

  domain_name <- getOption("xportr.domain_name")
  order_name <- getOption("xportr.order_name")
  variable_name <- getOption("xportr.variable_name")

  if (inherits(metadata, "Metacore")) metadata <- metadata$ds_vars

  if (domain_name %in% names(metadata) && !is.null(domain)) {
    metadata <- metadata %>%
      dplyr::filter(!!sym(domain_name) == .env$domain & !is.na(!!sym(order_name)))
  } else {
    metadata <- metadata %>%
      dplyr::filter(!is.na(!!sym(order_name)))
    # Common check for multiple variables name
    check_multiple_var_specs(metadata, variable_name)
  }

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

  df_re_ord
}

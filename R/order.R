#' Order variables of a dataset according to Spec
#'
#' @param .df A data frame of CDISC standard.
#' @param metadata A data frame containing variable level metadata.
#' @param domain A character value to subset the `.df`. If `NULL`(default), uses
#'   `.df` value as a subset condition.
#' @param verbose Option for messaging order results
#' @param metacore `r lifecycle::badge("deprecated")` Previously used to pass metadata now renamed with `metadata`
#'
#' @export
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
#' adsl <- xportr_order(adsl, metadata)
xportr_order <- function(
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
  order_name <- getOption("xportr.order_name")
  variable_name <- getOption("xportr.variable_name")

  ## Common section to detect domain from argument or pipes

  df_arg <- tryCatch(as_name(enexpr(.df)), error = function(err) NULL)
  domain <- get_domain(.df, df_arg, domain)
  if (!is.null(domain)) attr(.df, "_xportr.df_arg_") <- domain

  ## End of common section

  if (inherits(metadata, "Metacore")) {
    metadata <- metadata$ds_vars
  }

  if (domain_name %in% names(metadata)) {
    metadata <- metadata %>%
      dplyr::filter(!!sym(domain_name) == domain & !is.na(!!sym(order_name)))
  } else {
    metadata <- metadata %>%
      dplyr::filter(!is.na(!!sym(order_name)))
  }

  # Grabs vars from Spec and inputted dataset
  vars_in_spec_ds <- metadata[, c(variable_name, order_name)] %>%
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

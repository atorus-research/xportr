#' Assign Variable Label
#'
#' Assigns variable label from a variable level metadata to a given data frame.
#'
#' @param .df A data frame of CDISC standard.
#' @param metacore A data frame containing variable level metadata.
#' @param domain A character value to subset the `.df`. If `NULL`(default), uses
#'   `.df` value as a subset condition.
#' @param verbose The action the function takes when a variable length isn't
#'   Found. Options are 'stop', 'warn', 'message', and 'none'
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
#' metacore <- data.frame(
#'   dataset = "adsl",
#'   variable = c("USUBJID", "SITEID", "AGE", "SEX"),
#'   label = c("Unique Subject Identifier", "Study Site Identifier", "Age", "Sex")
#' )
#'
#' adsl <- xportr_label(adsl, metacore)
xportr_label <- function(.df, metacore, domain = NULL,
                         verbose = getOption("xportr.label_verbose", "none")) {
  
  domain_name <- getOption("xportr.domain_name")
  variable_name <- getOption("xportr.variable_name")
  variable_label <- getOption("xportr.label")
  
  df_arg <- as_name(enexpr(.df))
  
  if (!is.null(attr(.df, "_xportr.df_arg_"))) df_arg <- attr(.df, "_xportr.df_arg_")
  else if (identical(df_arg, ".")) {
    attr(.df, "_xportr.df_arg_") <- get_pipe_call()
    df_arg <- attr(.df, "_xportr.df_arg_") 
  }
  
  if (!is.null(domain) && !is.character(domain)) {
    abort(c("`domain` must be a vector with type <character>.",
            x = glue("Instead, it has type <{typeof(domain)}>."))
    )
  }
  
  df_arg <- domain %||% df_arg
  
  if (!is.null(domain)) attr(.df, "_xportr.df_arg_") <- domain
  
  if (inherits(metacore, "Metacore"))
    metacore <- metacore$var_spec
  
  if (domain_name %in% names(metacore)) {
    metadata <- metacore %>%
      dplyr::filter(!!sym(domain_name) == df_arg)
  } else {
    metadata <- metacore
  }

  
  # Check any variables missed in metadata but present in input data ---
  miss_vars <- setdiff(names(.df), metadata[[variable_name]])

  label_log(miss_vars, verbose)
  
  label <- metadata[[variable_label]]
  names(label) <- metadata[[variable_name]]
  
  # Check any variable label have more than 40 characters ---
  label_len <- lapply(label, nchar)
  err_len <- which(label_len > 40) %>% names
  
  if (length(err_len) > 0) {
    warn(
      c("Length of variable label must be 40 characters or less.",
      x = glue("Problem with {encode_vars(err_len)}."))
    )
  }
  
  for (i in names(.df)) {
    if (i %in% miss_vars) attr(.df[[i]], "label") <- ""
    else attr(.df[[i]], "label") <- label[[i]]
  }
  
  .df
}
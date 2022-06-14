#' Assign SAS Length
#'
#' Assigns SAS length from a variable level metadata to a given data frame.
#'
#' @param .df A data frame of CDISC standard.
#' @param metacore A data frame containing variable level metadata.
#' @param domain A character value to subset the `.df`. If `NULL`(default), uses
#'   `.df` value as a subset condition.
#' @param verbose The action the function takes when a length isn't found in 
#'   metadata. Options are 'stop', 'warn', 'message', and 'none'
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
#' metacore <- data.frame(
#'   dataset = c("adsl", "adsl"),
#'   variable = c("USUBJID", "BRTHDT"),
#'   length = c(10, 8)
#' )
#' 
#' adsl <- xportr_length(adsl, metacore)
xportr_length <- function(.df, metacore, domain = NULL,
                          verbose = getOption("xportr.length_verbose", "none")) {
  
  domain_name <- getOption("xportr.domain_name")
  variable_length <- getOption("xportr.length")
  variable_name <- getOption("xportr.variable_name")
  
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
  if (first_class(col) %in% "character") 200
  else 8
}

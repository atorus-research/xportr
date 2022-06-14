#' Assign SAS Format
#'
#' Assigns a SAS format from a variable level metadata to a given data frame.
#'
#' @param .df A data frame of CDISC standard.
#' @param metacore A data frame containing variable level metadata.
#' @param domain A character value to subset the `.df`. If `NULL`(default), uses
#'   `.df` value as a subset condition.
#' @param verbose The action the function takes when a variable label isn't.
#'   found. Options are 'stop', 'warn', 'message', and 'none'
#'
#' @return Data frame with `SASformat` attributes for each variable.
#' @family metadata functions
#' @seealso [xportr_label()], [xportr_df_label()] and [xportr_length()]
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
#'   format = c(NA, "DATE9.")
#' )
#'
#' adsl <- xportr_format(adsl, metacore)
xportr_format <- function(.df, metacore, domain = NULL, verbose = getOption("xportr.format_verbose", "none")) {
  
  domain_name <- getOption("xportr.domain_name")
  format_name <- getOption("xportr.format_name")
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
      dplyr::filter(!!sym(domain_name) == df_arg & !is.na(!!sym(format_name)))
  } else {
    metadata <- metacore
  }

    
  format <- metadata %>%
    select(!!sym(format_name))  %>%
    unlist() %>%
    toupper()
  
  names(format) <- metadata[[variable_name]]
  
  for (i in names(format)) {
    attr(.df[[i]], "format.sas")  <- format[[i]]
  }
  
  .df
}
#' Order variables of a dataset according to Spec
#'
#' @param .df A data frame of CDISC standard.
#' @param datadef A data frame containing variable level metadata.
#' @param domain A character value to subset the `.df`. If `NULL`(default), uses
#'   `.df` value as a subset condition.
#' @param verbose Option for messaging order results
#' 
#' @export
#' @return Dataframe that has been re-ordered according to spec
#' 
xportr_order <- function(.df, datadef, domain = NULL, verbose = getOption("xportr.order_verbose", "none")) {
  
  domain_name <- getOption("xportr.domain_name")
  order_name <- getOption("xportr.order_name")
  variable_name <- getOption("xportr.variable_name")
  
  
  df_arg <- as_name(enexpr(.df))
  
  if (!is.null(attr(.df, "_xportr.df_arg_"))) df_arg <- attr(.df, "_xportr.df_arg_")
  else if(identical(df_arg, ".")){
    attr(.df, "_xportr.df_arg_") <- get_pipe_call()
    df_arg <- attr(.df, "_xportr.df_arg_") 
  }
  
  if (!is.null(domain) && !is.character(domain)) {
    abort(c("`domain` must be a vector with type <character>.",
            x = glue("Instead, it has type <{typeof(domain)}>."))
    )
  }
  
  df_arg <- domain %||% df_arg
  
  if(!is.null(domain)) attr(.df, "_xportr.df_arg_") <- domain
  
  if (inherits(datadef, "Metacore"))
    datadef <- datadef$ds_vars
  
  metadata <- datadef %>%
    dplyr::filter(!!sym(domain_name) == df_arg & !is.na(!!sym(order_name)))
  
  # Grabs vars from Spec and inputted dataset
  vars_in_spec_ds <- metadata[,variable_name]
  
  # Grabs all variables from Spec file and orders accordingly
  ord_vars <- .df %>% 
    select(all_of(vars_in_spec_ds)) %>%
    arrange(sym(!!order_name))
  
  # Variables not in Spec file - will be moved to the end
  drop_vars <- .df %>% 
    select(!all_of(vars_in_spec_ds))
  
  # Used in warning message for how many vars have been moved
  moved_vars <- nrow(drop_vars)
  ordered_vars <- nrow(ord_vars)
  
  df_re_ord <- bind_cols(ord_vars, drop_vars)
  
  # Function is located in messages.R
  var_ord_msg(moved_vars, verbose)  
  
  return(df_re_ord)
}







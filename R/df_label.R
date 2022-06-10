#' Assign Dataset Label
#'
#' Assigns dataset label from a dataset level metadata to a given data frame.
#'
#' @param .df A data frame of CDISC standard.
#' @param metacore A data frame containing dataset level metadata.
#' @param domain A character value to subset the `.df`. If `NULL`(default), uses
#'   `.df` value as a subset condition.
#'
#' @return Data frame with label attributes.
#' @family metadata functions
#' @seealso [xportr_label()], [xportr_format()] and [xportr_length()]
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
#'   dataset = c("adsl", "adae"),
#'   label = c("Subject-Level Analysis", "Adverse Events Analysis")
#' )
#'
#' adsl <- xportr_df_label(adsl, metacore)
xportr_df_label <- function(.df, metacore, domain = NULL) {
  
  domain_name <- getOption("xportr.df_domain_name")
  label_name <- getOption("xportr.df_label")
  
  
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
  
  if (inherits(metacore, "Metacore"))
    metacore <- metacore$ds_spec
  
  label <- metacore %>%
    filter(!!sym(domain_name) == df_arg) %>%
    select(!!sym(label_name)) %>%
    # If a dataframe is used this will also be a dataframe, change to character.
    as.character()
  
  label_len <- nchar(label)
  
  if (label_len > 40) {
    abort("Length of dataset label must be 40 characters or less.")
  }
  
  
  attr(.df, "label") <- label
  
  .df
}


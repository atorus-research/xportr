#' Utility Logging Function
#' 
#' Functions to output user messages, usually relating to differences
#' found between dataframe and the metacore/metadata object
#'
#' @param message Output to be sent out for user
#' @param type Three types: abort, warn, inform
#' @param ... additional arguments if needed
#'
#' @return Output to Console
#' @export
xportr_logger <- function(message, type = "none", ...) {
  
  log_fun <- switch(type,
                    stop = abort,
                    warn = warn,                   
                    message = inform,
                    return())
  
  do.call(log_fun, list(message, ...))

}

#' Utility for Renaming Variables
#'
#' @param tidy_names_df dataframe
#' @param verbose Provides additional messaging for user
#'
#' @return Output to Console
#' @export
var_names_log <- function(tidy_names_df, verbose){
  
  
  only_renames <- tidy_names_df %>%
    filter(original_varname != renamed_var) %>%
    mutate(renamed_msg = paste0("Var ", col_pos, ":  '", original_varname,
                                "' was renamed to '", renamed_var, "'"))
  
  # Message regarding number of variables that were renamed/ modified
  num_renamed <- nrow(only_renames)
  tot_num_vars <- nrow(tidy_names_df)
  message("\n")
  cli::cli_h2(paste0( num_renamed, " of ", tot_num_vars, " (",
                     round(100*(num_renamed/tot_num_vars), 1), "%) variables were renamed"))
  
  # Message stating any renamed variables each original variable and it's new name
  if (nrow(only_renames) > 0) message(paste0(paste(only_renames$renamed_msg, collapse = "\n"), "\n"))
  
  # Message checking for duplicate variable names after renamed (Pretty sure
  # this is impossible) but good to have a check none-the-less.
  dups <- tidy_names_df %>% filter(renamed_n > 1)
  if (nrow(dups) != 0) {
    cli::cli_alert_danger(
      paste("Duplicate renamed term(s) were created. Consider creating dictionary terms for:",
                  paste(unique(dups$renamed_var), collapse = ", ")
    ))
  }
}

#' Utility for Types
#'
#' @param meta_ordered fill in later
#' @param type_mismatch_ind fill in later
#' @param verbose Provides additional messaging for user
#'
#' @return Output to Console
#' @export
type_log <- function(meta_ordered, type_mismatch_ind, verbose){
  
  if (length(type_mismatch_ind) > 0) {
    
    message <- glue(
      "Variable type(s) in dataframe don't match metadata: ",
      paste0(glue("{encode_vars(meta_ordered[type_mismatch_ind, 'variable'])}"),
             collapse = "", sep = " ")
    )

    xportr_logger(message, verbose)
    
    cli_h2("Variable type mismatches found.")
    cli_alert_success("{ length(type_mismatch_ind) } variables coerced")
  }
}

#' Utility for Lengths
#'
#' @param miss_vars Variables missing from metatdata 
#' @param verbose Provides additional messaging for user
#'
#' @return Output to Console
#' @export
length_log <- function(miss_vars, verbose) {
  
  if (length(miss_vars) > 0) {
    
    cli_h2("Variable lengths missing from metadata.")
    cli_alert_success("{ length(miss_vars) } lengths resolved")
    
    xportr_logger(
      c("Variable(s) present in dataframe but doesn't exist in `metadata`.",
        x = glue("Problem with {encode_vars(miss_vars)}")),
      type = verbose
    )
  }
}

#' Utility for Variable Labels
#'
#' @param miss_vars Missing variables in metadata
#' @param verbose Provides additional messaging for user
#'
#' @return Output to Console
#' @export
label_log <- function(miss_vars, verbose){
  if (length(miss_vars) > 0) {
    
    cli_h2("Variable labels missing from metadata.")
    cli_alert_success("{ length(miss_vars) } labels skipped")
    
    xportr_logger(
      c("Variable(s) present in dataframe but doesn't exist in `metadata`.",
        x = glue("Problem with {encode_vars(miss_vars)}")),
      type = verbose
    )
  }
}


#' Utility for Ordering
#'
#' @param moved_vars Variables moved in the dataset
#' @param verbose Provides additional messaging for user
#'
#' @return Output to Console
#' @export
var_ord_msg <- function(moved_vars, verbose){
  
  if (moved_vars > 0) {
    cli_h2("{ length(moved_vars) } variables not in spec and moved to end")
    message <- glue(
      "Variable reordered in `.df`: ",
      paste0(glue("{ encode_vars(moved_vars) }"),
             collapse = "", sep = " ")
    )
    xportr_logger(message, verbose)
  } else {
    cli_h2("All variables in specification file are in dataset")
  }

}

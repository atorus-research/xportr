
#' Title
#'
#' @param message fill in later
#' @param type fill in later
#' @param ... fill in later
#'
#' @return stuff
#' @export
xportr_logger <- function(message, type = "none", ...) {
  
  log_fun <- switch(type,
                    stop = abort,
                    warn = warn,                   
                    message = inform,
                    return())
  
  do.call(log_fun, list(message, ...))

}

## Functions to output user messages, usually relating to differences
## found between .df and the metacore object

#' Title
#'
#' @param tidy_names_df fill in later
#' @param verbose fill in later
#'
#' @return stuff
#' @export
var_names_log <- function(tidy_names_df, verbose){
  
  
  only_renames <- tidy_names_df %>%
    filter(original_varname != renamed_var) %>%
    mutate(renamed_msg = paste0("Var ", col_pos, ":  '", original_varname,
                                "' was renamed to '", renamed_var, "'"))
  
  # Message regarding number of variables that were renamed/ modified
  num_renamed <-nrow(only_renames)
  tot_num_vars <- nrow(tidy_names_df)
  message("\n")
  cli::cli_h2(paste0( num_renamed, " of ", tot_num_vars, " (",
                     round(100*(num_renamed/tot_num_vars), 1), "%) variables were renamed"))
  
  # Message stating any renamed variables each original variable and it's new name
  if(nrow(only_renames) > 0) message(paste0(paste(only_renames$renamed_msg, collapse = "\n"), "\n"))
  
  # Message checking for duplicate variable names after renamed (Pretty sure
  # this is impossible) but good to have a check none-the-less.
  dups <- tidy_names_df %>% filter(renamed_n > 1)
  if(nrow(dups) != 0) {
    cli::cli_alert_danger(
      paste("Duplicate renamed term(s) were created. Consider creating dictionary terms for:",
                  paste(unique(dups$renamed_var), collapse = ", ")
    ))
  }
}

#' Title
#'
#' @param meta_ordered fill in later
#' @param type_mismatch_ind fill in later
#' @param verbose fill in later
#'
#' @return stuff
#' @export
type_log <- function(meta_ordered, type_mismatch_ind, verbose){
  
  if(length(type_mismatch_ind) > 0) {
    
    message <- glue(
      "Variable type(s) in `.df` don't match metadata: ",
      paste0(glue("{encode_vars(meta_ordered[type_mismatch_ind, 'variable'])}"),
             collapse = "", sep = " ")
    )

    xportr_logger(message, verbose)
    
    cli_h2("Variable type mismatches found.")
    cli_alert_success("{ length(type_mismatch_ind) } variables coerced")
  }
}

#' Title
#'
#' @param miss_vars fill in later
#' @param verbose fill in later
#'
#' @return stuff
#' @export
length_log <- function(miss_vars, verbose) {
  
  if (length(miss_vars) > 0) {
    
    cli_h2("Variable lengths missing from metadata.")
    cli_alert_success("{ length(miss_vars) } lengths resolved")
    
    xportr_logger(
      c("Variable(s) present in `.df` but doesn't exist in `metacore`.",
        x = glue("Problem with {encode_vars(miss_vars)}")),
      type = verbose
    )
  }
}

#' Title
#'
#' @param miss_vars fill in later
#' @param verbose fill in later
#'
#' @return stuff
#' @export
label_log <- function(miss_vars, verbose){
  if (length(miss_vars) > 0) {
    
    cli_h2("Variable labels missing from metadata.")
    cli_alert_success("{ length(miss_vars) } labels skipped")
    
    xportr_logger(
      c("Variable(s) present in `.df` but doesn't exist in `metacore`.",
        x = glue("Problem with {encode_vars(miss_vars)}")),
      type = verbose
    )
  }
}


#' Title
#'
#' @param moved_vars fill in later
#' @param verbose fill in later
#'
#' @return stuff
#' @export
var_ord_msg <- function(moved_vars, verbose){
  
  # if (moved_vars > 0) {
  #   cli_alert_info(c(
  #     "I have orderd {ordered_vars} variables according to {vendor} {df1} Spec and moved {moved_vars} variables that were not in the {vendor} {df1} Spec to the end of {df1} dataset"))
  #   
  # } else if (moved_vars == 0){
  #   cli_alert_info(c(
  #     "Zero variables were ordered according to {vendor} {tab_model} {df1} Spec for {df1}"))
  # }
  # 
  # else {
  #   xportr_logger("Opps! Something went wrong...", type = "stop")
  # }
  
  
  if(moved_vars > 0){
    cli_h2("{ length(moved_vars) } variables reordered")
    message <- glue(
      "Variable reordered in `.df`: ",
      paste0(glue("{ encode_vars(moved_vars) }"),
             collapse = "", sep = " ")
    )
    xportr_logger(message, verbose)
  } else {
    cli_h2("Variables already in correct order")
  }

}

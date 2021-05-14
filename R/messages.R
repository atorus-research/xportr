
xportr_logger <- function(message, type = "none", ...) {
  
  log_fun <- switch(type,
                    stop = abort,
                    warn = warn,                   
                    message = inform,
                    return())
  
  do.call(log_fun, list(message, ...))

}

## Function to output user messages
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

length_log <- function(miss_vars, verbose) {
  
  if (length(miss_vars) > 0) {
    
    cli_h2("Variable lengths missing from metadata.")
    cli_alert_success("{ length(miss_vars) } lengths resolved")
    
    xportr_logger(
      c("Variable(s) present in `.df` but doesn't exist in `datadef`.",
        x = glue("Problem with {encode_vars(miss_vars)}")),
      type = verbose
    )
  }
}

label_log <- function(miss_vars, verbose){
  if (length(miss_vars) > 0) {
    
    cli_h2("Variable labels missing from metadata.")
    cli_alert_success("{ length(miss_vars) } labels skipped")
    
    xportr_logger(
      c("Variable(s) present in `.df` but doesn't exist in `datadef`.",
        x = glue("Problem with {encode_vars(miss_vars)}")),
      type = verbose
    )
  }
}




# --------------------------------------
##   ===  Check Variable Order part  ===
# --------------------------------------

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

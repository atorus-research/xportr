
xportr_logger <- function(message, type = "none", ...) {
  
  log_fun <- switch(type,
                    stop = rlang::abort,
                    warn = rlang::warn,
                    message = cli::cli_alert_info,
                    return())
  
  do.call(log_fun, list(message, ...))
  
}


## Function to output user messages
type_log <- function(meta_ordered, type_mismatch_ind, verbose){
  
  cli_h2("Variable type mismatches found.")
  cli_alert_success("{ length(type_mismatch_ind) } variables coerced")
  
  if(length(type_mismatch_ind) > 0) {
    
    message <- glue(
      "Variable(Table)[Metadata]: \n",
      paste0(glue("{meta_ordered[type_mismatch_ind, 'variable']}",
                  "({meta_ordered[type_mismatch_ind, 'type.x']})",
                  "[{meta_ordered[type_mismatch_ind, 'type.y']}]"),
             collapse = "", sep = "\n")
    )
    
    xportr_logger(message, verbose)
  }
}


## Function to output user messages
type_log <- function(meta_ordered, type_mismatch_ind, verbose){
  
  if(length(type_mismatch_ind) > 0) {
    
    message <- glue(
      "Variable(Table)[Metadata]: \n",
      paste0(glue("{meta_ordered[type_mismatch_ind, 'variable']}",
                  "({meta_ordered[type_mismatch_ind, 'type.x']})",
                  "[{meta_ordered[type_mismatch_ind, 'type.y']}]"),
             collapse = "", sep = "\n")
    )
    
    cli_h2("Variable type mismatches found.")
    cli_alert_success("{ length(type_mismatch_ind) } variables coerced")
    
    if(verbose == "stop") abort(message)
    else if (verbose == "warn") warn(message)
    else if (verbose == "message") cli_alert_info(message)
    
    
  }
  
}

xportr_logger <- function(message, type) {
  
  log_fun <- switch(type,
                    stop = rlang::abort,
                    warn = rlang::warn,
                    message = cli::cli_alert_info,
                    return())
  
  do.call(log_fun, list(message))
  
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

# -------------------------------
##   ===  Check Core part  ===
# -------------------------------

core_log <- function (type, miss_list_any, miss_list_all, vars_not_in_dataset, ds, verbose){

  # Update emphasis color for highlighting variable names within the lines of text.
  cli::cli_div(theme = list(span.emph = list(color = "orange")))

  # <--- Required vars ---> #
  if (type == 'req'){
    cli::cli_h3("Checking Required variables...")
    # If ANY 'req' variable is missing or has NA - stop.
    if (length(vars_not_in_dataset) > 0) {
      message <- c("Required variable(-s) {.emph ", paste0(vars_not_in_dataset, collapse = ' '), "} are not present in ", str_to_upper(ds), ".")
      xportr_logger(message, type = "stop")
    }
    if (length(miss_list_any) > 0){
      message <- c("Required variable(-s) {.emph ", paste0(miss_list_any, collapse = ' '), "} in ", ds, " contains missing values!
      Required variables must always be included in the dataset and cannot be null for any record. Please refer to
      https://www.cdisc.org/ for more details.")
      xportr_logger(message, type = "stop")
    }
    cli::cli_alert_success("Check done.")
  }

  # <--- Expected vars ---> #
  if (type == 'exp'){
    cli::cli_h3("Checking Expected variables...")
    # If ANY 'exp' variable is missing - stop.
    if (length(vars_not_in_dataset) > 0) {
      message <- c("Expected variable(-s) {.emph ", paste0(vars_not_in_dataset, collapse = ' '), "} are not present in ", str_to_upper(ds), ". When the study
      does not include the data item for an expected variable, however, a null column must still be included in the
      dataset, and a comment must be included in the Define-XML document to state that the study does not include
      the data item. Please refer to https://www.cdisc.org/ for more details.")
      xportr_logger(message, type = verbose)
    }
    # If ALL values of the expected variable are NA - put a warning.
    if (length(miss_list_all) > 0){
      message <- c("Expected variable(-s) {.emph ", paste0(miss_list_all, collapse = ' '), "} in ", str_to_upper(ds), " has only NA values.
      Make sure to include comment in the Define-XML document to state that the study does not include the
      data item for the particular expected variable. Please refer to https://www.cdisc.org/ for more details.")
      xportr_logger(message, type = verbose)
    }
    cli::cli_alert_success("Check done.")
  }

  # <--- Permissible vars ---> #
  if (type == 'perm'){
    cli::cli_h3("Checking Permissible variables...")
    # If ALL values of the permissible variable are NA - put a warning.
    if (length(miss_list_all) > 0){
      message <- c("Permissible variable(-s) {.emph ", paste0(miss_list_all, collapse = ' '), "} in ", str_to_upper(ds), " has only NA values.
      Check if a study includes a data item that is represented in the particular permissible variable. If yes -
      make sure to include comment in the Define-XML document to indicate no data were available for that variable.
      Otherwise - remove the variable from dataset. Please refer to https://www.cdisc.org/ for more details.")
      xportr_logger(message, type = verbose)
    }
    cli::cli_alert_success("Check done.")
  }

  # <--- Conditionally required vars ---> #
  if (type == 'cond'){
    cli::cli_h3("Checking Conditionally Required variables...")
    # If ALL values of the conditionally required variable are NA - put a warning.
    if (length(miss_list_all) > 0){
      message <- c("Conditionally required variable(-s) {.emph ", paste0(miss_list_all, collapse = ' '), "} in ", str_to_upper(ds), " has
      only NA values.
      Check if a study includes a data item that is represented in the particular variable. If yes -
      make sure to include comment in the Define-XML document to indicate no data were available for that variable.
      Otherwise - remove the variable from dataset. Please refer to https://www.cdisc.org/ for more details.")
      xportr_logger(message, type = verbose)
    }
    else if (length(miss_list_any) > 0){
      message <- c("Conditionally required variable(-s) {.emph ", paste0(miss_list_any, collapse = ' '), "} in ", str_to_upper(ds), " contains missing values!
      Check if a study includes a data item that is represented in the particular variable. Please refer to
      https://www.cdisc.org/ for more details.")
      xportr_logger(message, type = verbose)
    }
    cli::cli_alert_success("Check done.")
  }

  # Close cli container. Reset my change to emphasis settings.
  cli::cli_end()

}




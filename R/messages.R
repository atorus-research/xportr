
xportr_logger <- function(message, type = "none", ...) {
  
  log_fun <- switch(type,
                    stop = xportr_stop,
                    warn = xportr_warn,                   
                    message = cli_this,
                    return())
  
  do.call(log_fun, list(message, ...))

}

# inspired by
# https://github.com/r-lib/gargle/blob/e2c7a48c208c3904d9038e8a7fd8d1045a4b3455/R/ui.R#L116
cli_this <-  function(..., .envir = parent.frame()) {
  txt <- cli::cli_format_method(cli::cli_text(..., .envir = .envir))
  txt
}

xportr_stop = function(..., .envir = parent.frame()) {
  txt <- purrr::map_chr(..., cli_this, .envir = .envir)
  
  if (length(txt) > 1L) {
    paste(txt[[1]], rlang::format_error_bullets(txt[-1]), sep = "\n") %>% 
      rlang::abort()
  }
  
  rlang::abort(txt)
}

xportr_warn = function(..., .envir = parent.frame()) {
  txt <- purrr::map_chr(..., cli_this, .envir = .envir)
  
  if (length(txt) > 1L) {
    paste(txt[[1]], rlang::format_error_bullets(txt[-1]), sep = "\n") %>% 
      rlang::warn()
  }
  
  rlang::warn(txt)
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

    if (verbose != "none"){
      cli::cli_h3("Checking Required variables...")
    }

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

    if (verbose != "none"){
      cli::cli_alert_success("Check done.")
    }
  }

  # <--- Expected vars ---> #
  if (type == 'exp'){

    if (verbose != "none"){
      cli::cli_h3("Checking Expected variables...")
    }

    # If ANY 'exp' variable is missing - stop.
    if (length(vars_not_in_dataset) > 0) {
      message <- c("Expected variable(-s) {.emph ", paste0(vars_not_in_dataset, collapse = ' '), "} are not present in ", str_to_upper(ds), ". When the study
      does not include the data item for an expected variable, however, a null column must still be included in the
      dataset, and a comment must be included in the Define-XML document to state that the study does not include
      the data item. Please refer to https://www.cdisc.org/ for more details.")
      xportr_logger(message, type = "stop")
    }
    # If ALL values of the expected variable are NA - put a warning.
    if (length(miss_list_all) > 0){
      message <- c("Expected variable(-s) {.emph ", paste0(miss_list_all, collapse = ' '), "} in ", str_to_upper(ds), " has only NA values.
      Make sure to include comment in the Define-XML document to state that the study does not include the
      data item for the particular expected variable. Please refer to https://www.cdisc.org/ for more details.")
      xportr_logger(message, type = verbose)
    }

    if (verbose != "none"){
      cli::cli_alert_success("Check done.")
    }
  }

  # <--- Permissible vars ---> #
  if (type == 'perm'){

    if (verbose != "none"){
      cli::cli_h3("Checking Permissible variables...")
    }
    # If ALL values of the permissible variable are NA - put a warning.
    if (length(miss_list_all) > 0){
      message <- c("Permissible variable(-s) {.emph ", paste0(miss_list_all, collapse = ' '), "} in ", str_to_upper(ds), " has only NA values.
      Check if a study includes a data item that is represented in the particular permissible variable. If yes -
      make sure to include comment in the Define-XML document to indicate no data were available for that variable.
      Otherwise - remove the variable from dataset. Please refer to https://www.cdisc.org/ for more details.")
      xportr_logger(message, type = verbose)
    }

    if (verbose != "none"){
      cli::cli_alert_success("Check done.")
    }
  }

  # <--- Conditionally required vars ---> #
  if (type == 'cond'){

    if (verbose != "none"){
      cli::cli_h3("Checking Conditionally Required variables...")
    }
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

    if (verbose != "none"){
      cli::cli_alert_success("Check done.")
    }
  }

  # Close cli container. Reset my change to emphasis settings.
  cli::cli_end()

}


# --------------------------------------
##   ===  Check Variable Order part  ===
# --------------------------------------

var_order_msg_alert <- function(df1, vendor, tab_model) {
  
  cli_div(theme = list(span.emph = list(color = "orange")))
  cli_alert_success("I have retrieved the {.emph {vendor}} {.emph {tab_model}} Spec for you.")
  
  cli_h2("Starting to Order Variables according to {.emph {vendor}} {.emph {tab_model}} {df1} Spec")
  cli_text("")
  
}

var_ord_msg_success <- function(df1, ordered_vars, moved_vars, vendor, tab_model){
  
  if (moved_vars > 0) {
    cli_alert_info(c(
      "I have orderd {ordered_vars} variables according to {vendor} {df1} Spec and moved {moved_vars} variables that were not in the {vendor} {df1} Spec to the end of {df1} dataset"))
    
  } else if (moved_vars == 0){
    cli_alert_info(c(
      "Zero variables were ordered according to {vendor} {tab_model} {df1} Spec for {df1}"))
  }
  
  else {
    xportr_logger("Opps! Something went wrong...", type = "stop")
  }
  
    xportr_logger(message, verbose)
}

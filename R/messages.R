
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
  
}


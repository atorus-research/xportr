
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
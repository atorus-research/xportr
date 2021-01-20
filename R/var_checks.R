#' UPPER CASE check
#'
#' Check to see if data set has all upper case for variables
#' @param .data Data frame containing data to calculate summary statistics for
#' @importFrom purrr map map2 reduce
#' @importFrom rlang quos quo_name quo_is_null enquo
#' @return Tibble of Variables flagged with lower case
#' @export
#'
#' @examples
check_var_case <- function(.data){
    bind_cols(colnames(.data), 
            str_detect(colnames(.data), "[A-Z0-9]+(?:[A-Z0-9]+)+")) %>% 
    rename("Variable"=...1, "Flag"=...2) %>% 
    filter(Flag == "FALSE")
}

#' Variable Checks 2
#'
#' @return
#' @export
#'
#' @examples
function(){
  
}

#' Variable Checks 3
#'
#' @return
#' @export
#'
#' @examples
function(){
  
}
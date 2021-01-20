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

#' Extract a label from a vector
#'
#' Extract a label from a vector
#' @param x A vector with a \"label\" attribute
#' @return The text label associated with the vector or `NA`
#' @keywords internal
extract_label <- function(x) {
  label <- try(attr(x, "label"))
  if (class(label) != "try-error") {
    label
  }
  else {
    NA
  }
}

#' Extract all labels from a Dataframe
#'
#' @param .data Dataframe with labels to extract
#' @importFrom purrr map
#' @importFrom magrittr %>%
#'
#' @return A vector of variable and labels for a dataframe
#' @export
#' @examples
#' data(dm)
#' labs <- extract_labels(dm)
extract_labels <- function(.data) {
  .data %>%
    map(extract_label) %>%
    unlist()
}

#' Variable Label Check
#'
#' Check for variable labels greater than 40 character length
#' @param .data Data set with labels to check
#' @return 
#' @export
#'
#' @examples
check_label_length <- function(.data){
  extract_labels(.data) %>% 
    as.list() %>% 
    as_tibble() %>% 
    pivot_longer(everything()) %>% 
    mutate(flag = if_else(str_length(value) > 40, TRUE, FALSE),
           label_length = str_length(value)) %>% 
    filter(flag == TRUE) %>% 
    select(-flag)
}

#' Variable Checks 3
#'
#' @return
#' @export
#'
#' @examples
function(){
  
}
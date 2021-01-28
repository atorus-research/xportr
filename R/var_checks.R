#' UPPER CASE check
#'
#' Check to see if data set has all upper case for variables
#' @param .data Data frame containing data to calculate summary statistics for
#' @importFrom dplyr rename filter
#' @importFrom stringr str_detect
#' @importFrom magrittr %>%
#' @return Tibble of Variables flagged with lower case
#' @export
#'
#' @examples
check_var_case <- function(.data){
    bind_cols(colnames(.data), 
            str_detect(colnames(.data), "[A-Z0-9]+(?:[A-Z0-9]+)+")) %>% 
    rename("Variable"=...1, "Flag"=...2) %>% 
    filter(Flag == FALSE)
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
#' @importFrom dplyr rename filter select 
#' @importFrom stringr str_length
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @return 
#' @export
#'
#' @examples
check_label_length <- function(.data){
  extract_labels(.data) %>% 
    as.list() %>% 
    as_tibble() %>% 
    pivot_longer(everything()) %>% 
    mutate(Flag = ifelse(str_length(value) > 40, TRUE, FALSE),
           label_length = str_length(value)) %>% 
    filter(Flag == TRUE) %>% 
    select(-Flag)
  
# Return Message with All LAbels Checked
}

#' ASCII Check on Variable Labels
#'
#' Check that only ASCII characters are being used in variable names and in variable labels
#' param .data
#' @importFrom xfun is.ascii
#' @importFrom magrittr %>%
#' @importFrom dplyr rename filter select 
#' @importFrom stringr str_length
#' @importFrom tidyr pivot_longer
#' @return
#' @export
#'
#' @examples
check_ascii_lbls <- function(.data){
  extract_labels(.data) %>% 
    as.list() %>% 
    as_tibble() %>% 
    pivot_longer(everything()) %>% 
    mutate(Flag = ifelse(is_ascii(value) == FALSE, "non-ASCII Found", "All ASCII")) %>% 
    filter(Flag == "non-ASCII Found")
}

check_ascii_lbls(teams_lbls)

is_ascii("À")

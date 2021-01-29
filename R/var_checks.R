#' Variable Length Check
#'
#' Check to see if variables in data all have length less than or equal to 8
#' @param .data Data frame containing data to calculate summary statistics for
#' @importFrom dplyr rename filter
#' @importFrom stringr str_detect
#' @importFrom magrittr %>%
#' @return Tibble of Variables flagged with lower case
#' @export
#'
#' @examples
check_var_length <- function(.data){
    
    chk_data <- as_tibble(colnames(.data)) %>% 
    mutate(flag = ifelse(str_length(value) > 8, TRUE, FALSE),
           var_length = str_length(value)) %>% 
    filter(flag == TRUE) %>% 
    select(-flag)
  
    if(nrow(chk_data) != 0){
    message("WARNING!! Xportr has detected variables in ", deparse(substitute(.data)), " with lengths greater than 8")
    }else{
    message("SUCCESS!! Xport did NOT detect any variables in ", deparse(substitute(.data)), " with lengths greater than 8")
    }
}

check_var_length(adsl_renamed)
check_var_length(adsl)

#' UPPER CASE Check
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
    rename("value"=...1, "flag"=...2) %>% 
    filter(flag == FALSE)
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

#' All label to a vector
#'
#' @param .data Dataframe with labels to extract
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @return A vector of variable and labels for a dataframe
#' @keywords internal
add_label <- function(x, label) {
  if (length(label) == 0) {
    label <- NULL
  }
  attr(x, "label") <- label
  x
}

#' Add labels to a Dataframe
#'
#' @param .data Dataframe with labels to extract
#' @importFrom purrr map map2
#' @importFrom magrittr %>%
#'
#' @return A vector of variable and labels for a dataframe
#' @export
#' @examples
add_labels <- function(.data, ...) {
  name_list <- c(...)
  df <- tibble(col = names(name_list), lab = name_list)
  .data %>%
    purrr::map2(names(.data), function(x, name) {
      label <- df %>%
        filter(col == name) %>%
        pull(lab) %>%
        unname()
      if(length(label) > 0) {
        add_label(x, label)
      } else {
        x
      }
    }) %>%
    as_tibble()
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
    mutate(flag = ifelse(str_length(value) > 40, TRUE, FALSE),
           label_length = str_length(value)) %>% 
    filter(flag == TRUE) %>% 
    select(-flag)
  
# Return Message with All LAbels Checked
}

#' ASCII Check on Variable Names
#'
#' Check that only ASCII characters are being used in variable names
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
check_ascii_vars <- function(.data){
  
  as_tibble(colnames(.data)) %>%  
              mutate(flag = ifelse(is_ascii(colnames(.data)) == FALSE, "non-ASCII Found", "All ASCII"))
    }

check_ascii_vars(teams_lbls)

#' ASCII Check on Variable Labels
#'
#' Check that only ASCII characters are being used in variable labels
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

# Return where non-ascii character is located?

is_ascii("À")

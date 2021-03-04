#' Variable Length Check
#'
#' Check to see if variables in data all have length less than or equal to 8
#'
#' @param .df Data frame
#'
#' @importFrom dplyr rename filter mutate select
#' @importFrom stringr str_detect
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#'
#' @return A tibble with variable names >= 8 characters.
#'
#' @noRd
xpt_check_var_length <- function(.df){

  chk_data <- as_tibble(colnames(.df)) %>%
    mutate(flag = ifelse(str_length(value) > 8, TRUE, FALSE),
           var_length = str_length(value)) %>%
    filter(flag == TRUE) %>%
    select(-flag)

  # if(nrow(chk_data) != 0){
  # message("WARNING!! Xportr has detected variables in ", deparse(substitute(.data)), " with lengths greater than 8")
  # }else{
  # message("SUCCESS!! Xport did NOT detect any variables in ", deparse(substitute(.data)), " with lengths greater than 8")
  # }
}

#' UPPER CASE Check
#'
#' Check to see if data set has all upper case for variables
#'
#' @param .df Data frame containing data to calculate summary statistics for
#'
#' @importFrom dplyr rename filter
#' @importFrom stringr str_detect
#' @importFrom magrittr %>%
#'
#' @return Tibble of Variables flagged with lower case
#'
#' @noRd
xpt_check_var_case <- function(.df){
  bind_cols(colnames(.df),
            str_detect(colnames(.df), "[A-Z0-9]+(?:[A-Z0-9]+)+")) %>%
    rename("value"=...1, "flag"=...2) %>%
    filter(flag == FALSE)
}

#' @title Extract a label from a vector
#' @param x A vector with a \"label\" attribute
#' @return The text label associated with the vector or `NA`
#' @noRd

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
#' @param .df Dataframe with labels to extract
#'
#' @importFrom purrr map
#' @importFrom magrittr %>%
#'
#' @return A vector of variable and labels for a dataframe
#'
#' @noRd
extract_labels <- function(.df) {
  .df %>%
    map(extract_label)  %>%
    unlist()
}

#' Add label to a vector
#'
#' @param .df Dataframe with labels to extract
#'
#' @importFrom purrr map
#' @importFrom magrittr %>%
#'
#' @return A vector of variable and labels for a dataframe
#'
#' @noRd
add_label <- function(x, label) {
  if (length(label) == 0) {
    label <- NULL
  }
  attr(x, "label") <- label
  x
}

#' Add labels to a Tibble
#'
#' @param .df A tibble or dataframe that are in need of labels
#' @param ... additional abilities as needed
#'
#' @importFrom purrr map map2
#' @importFrom magrittr %>%
#' @importFrom dplyr pull filter
#' @importFrom tibble tibble
#'
#' @return A vector of variable and labels for a dataframe
#'
#' @noRd
add_labels <- function(.df, ...) {
  name_list <- c(...)
  df <- tibble(col = names(name_list), lab = name_list)
  purrr::imap_dfc(.df, function(.x, .y) {
    label <- df %>% filter(col == .y) %>% pull(lab) %>% unname()

    if (length(label) > 0)
      add_label(.x, label)
    else
      NA
  })
}

#' Variable Label Check
#'
#' Check for variable labels greater than 40 characters in length
#'
#' @param .df Data set with labels to check
#'
#' @importFrom dplyr rename filter select
#' @importFrom stringr str_length
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
xpt_check_label_length <- function(.df){
  extract_labels(.df) %>%
    as.list() %>%
    as_tibble() %>%
    pivot_longer(everything()) %>%
    mutate(flag = ifelse(str_length(value) > 40, TRUE, FALSE),
           label_length = str_length(value)) %>%
    filter(flag == TRUE) %>%
    select(-flag)

  # TODO Return Message with All LAbels Checked
}

#' Check if a character vector consists of entirely ASCII characters
#'
#' Converts the encoding of a character vector to \code{'ascii'}, and check if
#' the result is \code{NA}.  This is a copy of the function from the
#' \code{xfun} package.
#'
#' @param x A character vector.
#'
#' @return A logical vector indicating whether each element of the character
#'   vector is ASCII.
#'
#' @noRd
is_ascii <- function(x) {
  out = !is.na(iconv(x, to = 'ascii'))
  out[is.na(x)] = NA
  out
}

#' ASCII Check on Variable Names
#'
#' Check that only ASCII characters are being used in variable names
#'
#' @param .df Data frame
#'
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr rename filter select
#' @importFrom stringr str_length
#' @importFrom tidyr pivot_longer
xpt_check_ascii_vars <- function(.df){

  as_tibble(colnames(.df)) %>%
    mutate(flag = ifelse(is_ascii(value) == FALSE, "non-ASCII Found", "All ASCII")) %>%
    filter(flag == "non-ASCII Found")
}



#' ASCII Check on Variable Labels
#'
#' Check that only ASCII characters are being used in variable labels
#'
#' @param .df Data frame
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr rename filter select
#' @importFrom stringr str_length
#' @importFrom tidyr pivot_longer
xpt_check_ascii_lbls <- function(.df){

  lbl <- extract_labels(.df)
  tibble(name = names(lbl), value = lbl) %>%

    mutate(flag = ifelse(is_ascii(value) == FALSE, "non-ASCII Found", "All ASCII")) %>%

    filter(flag == "non-ASCII Found")
}


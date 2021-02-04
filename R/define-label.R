#' Variable label
#' 
#' Assigns variable label from a variable level metadata to a given data frame.
#'
#' @param x A data frame.
#' @param y A data frame containing variable level metadata.
#'
#' @return Data frame with label attributes for each variable.
#' @seealso [xportr::define_dflabel()]
#' @export
#'
#' @examples
#' adsl <- tibble::tribble(
#'  ~USUBJID, ~SITEID, ~AGE, ~SEX,
#'  1001    , 001    , 11  , "M",
#'  1002    , 002    , 10  , "F",
#'  1003    , 003    , 14  , "M"
#' )
#' 
#' varmeta <- tibble::tribble(
#'  ~dataset, ~variable, ~label,
#'  "adsl"  , "USUBJID", "Unique Subject Identifier",
#'  "adsl"  , "SITEID" , "Study Site Identifier",
#'  "adsl"  , "AGE"    , "Age",
#'  "adsl"  , "SEX"    , "Sex"
#'  )
#' 
#' adsl <- define_varlabel(adsl, varmeta)
define_varlabel <- function(x, y){
  
  arg <- rlang::as_name(rlang::enexpr(x))
  
  metadata <- rlang::set_names(y, tolower) %>% 
    dplyr::filter(.data$dataset == arg)
  
  # Check any variables missed in metadata but present in input data ---
  miss_vars <- setdiff(names(x), metadata$variable)
  
  if (length(miss_vars) > 0) {
    rlang::abort(
      c("Variable(s) present in input data but doesn't exist in metadata.",
        x = glue::glue("Problem with {encode_vars(miss_vars)}"))
    ) 
  }
  
  label <- metadata$label
  names(label) <- metadata$variable
  
  # Check any variable label have more than 40 characters ---
  label_len <- lapply(label, nchar)
  err_len <- which(label_len > 40) %>% names
  
  if (length(err_len) > 0) {
    rlang::abort(
      c("Length of variable label exceeds 40 character.",
        x = glue::glue("Problem with {encode_vars(err_len)}."))
    )
  }
  
  for (i in names(label)) {
    attr(x[[i]], "label") <- label[[i]]
  }
  
  x
}


#' Dataset label
#' 
#' Assigns dataset label from a dataset level metadata to a given data frame.
#'
#' @param x A data frame.
#' @param y A data frame containing dataset level metadata.
#'
#' @return Data frame with label attributes.
#' @seealso [xportr::define_varlabel()]
#' @export
#'
#' @examples
#' adsl <- tibble::tribble(
#'  ~USUBJID, ~SITEID, ~AGE, ~SEX,
#'  1001    , 001    , 11  , "M",
#'  1002    , 002    , 10  , "F",
#'  1003    , 003    , 14  , "M"
#' )
#' 
#' dsmeta <- tibble::tribble(
#'  ~name , ~label,
#'  "adsl", "Subject-Level Analysis",
#'  "adae", "Adverse Events Analysis"
#' )
#' 
#' adsl <- define_dflabel(adsl, dsmeta)
define_dflabel <- function(x, y) {
  arg <- rlang::as_name(rlang::enexpr(x))
  
  metadata <- rlang::set_names(y, tolower) %>% 
    dplyr::filter(.data$name == arg)
  
  label <- metadata$label
  
  label_len <- nchar(label)
  
  if (label_len > 40) {
    rlang::abort("Length of dataset label exceeds 40 character.")
  }

  attr(x, "label") <- label
  
  x
}


# Helper ------------------------------------------------------------------

encode_vars <- function(x) {
  if (is.character(x)) {
    x <- encodeString(x, quote = "`")
  }
  
  glue::glue_collapse(x, sep = ", ", last = if (length(x) <= 2) " and " else ", and ")
}

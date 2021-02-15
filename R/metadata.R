#' Variable label
#'
#' Assigns variable label from a variable level metadata to a given data frame.
#'
#' @param .df A data frame of CDISC standard.
#' @param datadef A data frame containing variable level metadata.
#'
#' @return Data frame with label attributes for each variable.
#' @family metadata functions
#' @seealso [xportr_dflabel()] and [xportr_format()]
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
#' datadef <- tibble::tribble(
#'  ~dataset, ~variable, ~label,
#'  "adsl"  , "USUBJID", "Unique Subject Identifier",
#'  "adsl"  , "SITEID" , "Study Site Identifier",
#'  "adsl"  , "AGE"    , "Age",
#'  "adsl"  , "SEX"    , "Sex"
#'  )
#'
#' adsl <- xportr_label(adsl, datadef)
xportr_label <- function(.df, datadef){

  arg <- as_name(enexpr(.df))

  metadata <- set_names(datadef, tolower) %>%
    dplyr::filter(.data$dataset == arg)

  # Check any variables missed in metadata but present in input data ---
  miss_vars <- setdiff(names(.df), metadata$variable)

  if (length(miss_vars) > 0) {
    abort(
      c("Variable(s) present in `.df` but doesn't exist in `datadef`.",
        x = glue("Problem with {encode_vars(miss_vars)}"))
    )
  }

  label <- metadata$label
  names(label) <- metadata$variable

  # Check any variable label have more than 40 characters ---
  label_len <- lapply(label, nchar)
  err_len <- which(label_len > 40) %>% names

  if (length(err_len) > 0) {
    abort(
      c("Length of variable label must be 40 characters or less.",
        .df = glue("Problem with {encode_vars(err_len)}."))
    )
  }

  for (i in names(label)) {
    attr(.df[[i]], "label") <- label[[i]]
  }

  .df
}

#' Dataset label
#'
#' Assigns dataset label from a dataset level metadata to a given data frame.
#'
#' @param .df A data frame of CDISC standard.
#' @param datadef A data frame containing dataset level metadata.
#'
#' @return Data frame with label attributes.
#' @family metadata functions
#' @seealso [xportr_label()] and [xportr_format()]
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
#' datadef <- tibble::tribble(
#'  ~name , ~label,
#'  "adsl", "Subject-Level Analysis",
#'  "adae", "Adverse Events Analysis"
#' )
#'
#' adsl <- xportr_df_label(adsl, dsmeta)
xportr_df_label <- function(.df, datadef) {
  arg <- as_name(enexpr(.df))

  metadata <- set_names(datadef, tolower) %>%
    dplyr::filter(.data$name == arg)

  label <- metadata$label

  label_len <- nchar(label)

  if (label_len > 40) {
    abort("Length of dataset label must be 40 characters or less.")
  }

  attr(.df, "label") <- label

  .df
}

#' SAS format
#'
#' Assigns SAS format from a variable level metadata to a given data frame.
#'
#' @param .df A data frame of CDISC standard.
#' @param datadef A data frame containing variable level metadata.
#'
#' @return Data frame with `SASformat` attributes for each variable.
#' @family metadata functions
#' @seealso [xportr::define_varlabel()] and [xportr::define_dflabel()]
#' @export
#'
#' @examples
#' adsl <- tibble::tribble(
#'  ~USUBJID, ~BRTHDT,
#'  1001    , 1,
#'  1002    , 1,
#'  1003    , 2
#' )
#'
#' datadef <- tibble::tribble(
#'  ~dataset, ~variable, ~sas_format,
#'  "adsl"  , "USUBJID", NA,
#'  "adsl"  , "BRTHDT" , "date9."
#'  )
#'
#' adsl <- xportr_format(adsl, datadef)
xportr_format <- function(.df, datadef) {

  # TODO: Extract Needed datadef dataframe

  arg <- as_name(enexpr(.df))

  metadata <- set_names(datadef, tolower) %>%
    dplyr::filter(.data$dataset == arg & !is.na(.data$sas_format))

  format <- toupper(metadata$sas_format)
  names(format) <- metadata$variable

  for (i in names(format)) {
    SASxport::SASformat(.df[[i]]) <- format[[i]]
  }

  .df
}

# define_length <- function(x, y) {
#
#   arg <- as_name(enexpr(x))
#
#   #-- Character only character variables --
#   metadata <- set_names(y, tolower) %>%
#     dplyr::filter(.data$dataset == arg, !toupper(.data$type) %in% c('INTEGER', 'FLOAT'))
#
#   # Check any variables missed in metadata but present in input data ---
#   miss_vars <- setdiff(names(x), metadata$variable)
#
#   if (length(miss_vars) > 0) {
#     abort(
#       c("Variable(s) present in `x` but doesn't exist in `y`.",
#         x = glue("Problem with {encode_vars(miss_vars)}"))
#     )
#   }
#
#   length <- metadata$length
#   names(length) <- metadata$variable
#
#   for (i in names(length)) {
#     SASxport::SASlength(x[[i]]) <- length[[i]]
#   }
#
#   x
# }

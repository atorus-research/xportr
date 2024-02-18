#' Assign SAS Format
#'
#' Assigns a SAS format from a variable level metadata to a given data frame. If
#' no format is found for a given variable, it is set as an empty character
#' vector. This is stored in the '`format.sas`' attribute.
#'
#' @inheritParams xportr_length
#'
#' @return Data frame with `SASformat` attributes for each variable.
#'
#' @section Format Checks: This function carries out a series of basic
#'  checks to ensure the formats being applied make sense.
#'
#' 1) If the variable has a suffix of `DT`, `DTM`, `TM` (indicating a
#'  numeric date/time variable) then a warning will be shown if there is
#'   no format associated with it.
#'
#' 2) If a variable is character then a warning will be shown if there is
#'  no `$` prefix in the associated format.
#'
#' 3) If a variable is character then a warning will be shown if the
#'  associated format has greater than 31 characters (excluding the `$`).
#'
#' 4) If a variable is numeric then a warning will be shown if there is a
#'  `$` prefix in the associated format.
#'
#' 5) If a variable is numeric then a warning will be shown if the
#'  associated format has greater than 32 characters.
#'
#' 6) All formats will be checked against a list of formats considered
#'  'standard' as part of an ADaM dataset. Note, however, this list is not
#'   exhaustive (it would not be feasible to check all the functions
#'    within the scope of this package). If the format is not found in the
#'     'standard' list, then a message is created advising the user to
#'      check.
#'
#' | \strong{Format Name} | \strong{w Values} | \strong{d Values}  |
#' |----------------------|-------------------|--------------------|
#' | w.d                  | 1 - 32            | ., 0 - 31          |
#' | $w.                  | 1 - 200           |                    |
#' | DATEw.               | ., 5 - 11         |                    |
#' | DATETIMEw.           | 7 - 40            |                    |
#' | DDMMYYw.             | ., 2 - 10         |                    |
#' | HHMM.                |                   |                    |
#' | MMDDYYw.             | ., 2 - 10         |                    |
#' | TIMEw.               | ., 2 - 20         |                    |
#' | WEEKDATEw.           | ., 3 - 37         |                    |
#' | YYMMDDw.             | ., 2 - 10         |                    |
#' | B8601DAw.            | ., 8 - 10         |                    |
#' | B8601DTw.d           | ., 15 - 26        | ., 0 - 6           |
#' | B8601TM.             |                   |                    |
#' | IS8601DA.            |                   |                    |
#' | IS8601TM.            |                   |                    |
#' | E8601DAw.            | ., 10             |                    |
#' | E8601DNw.            | ., 10             |                    |
#' | E8601DTw.d           | ., 16 - 26        | ., 0 - 6           |
#' | E8601DXw.            | ., 20 - 35        |                    |
#' | E8601LXw.            | ., 20 - 35        |                    |
#' | E8601LZw.            | ., 9 - 20         |                    |
#' | E8601TMw.d           | ., 8 - 15         | ., 0 - 6           |
#' | E8601TXw.            | ., 9 - 20         |                    |
#' | E8601TZw.d           | ., 9 - 20         | ., 0 - 6           |
#'
#' @section Metadata: The argument passed in the 'metadata' argument can either
#'   be a metacore object, or a data.frame containing the data listed below. If
#'   metacore is used, no changes to options are required.
#'
#'   For data.frame 'metadata' arguments three columns must be present:
#'
#'   1) Domain Name - passed as the 'xportr.domain_name' option. Default:
#'   "dataset". This is the column subset by the 'domain' argument in the
#'   function.
#'
#'   2) Format Name - passed as the 'xportr.format_name' option. Default:
#'   "format". Character values to update the '`format.sas`' attribute of the
#'   column. This is passed to `haven::write` to note the format.
#'
#'   3) Variable Name - passed as the 'xportr.variable_name' option. Default:
#'   "variable". This is used to match columns in '.df' argument and the
#'   metadata.
#'
#' @export
#'
#' @examples
#' adsl <- data.frame(
#'   USUBJID = c(1001, 1002, 1003),
#'   BRTHDT = c(1, 1, 2)
#' )
#'
#' metadata <- data.frame(
#'   dataset = c("adsl", "adsl"),
#'   variable = c("USUBJID", "BRTHDT"),
#'   format = c(NA, "DATE9.")
#' )
#'
#' adsl <- xportr_format(adsl, metadata, domain = "adsl")
xportr_format <- function(.df,
                          metadata = NULL,
                          domain = NULL,
                          metacore = deprecated()) {
  if (!missing(metacore)) {
    lifecycle::deprecate_stop(
      when = "0.3.1.9005",
      what = "xportr_format(metacore = )",
      with = "xportr_format(metadata = )"
    )
  }

  ## Common section to detect default arguments

  domain <- domain %||% attr(.df, "_xportr.df_arg_")
  if (!is.null(domain)) attr(.df, "_xportr.df_arg_") <- domain

  metadata <- metadata %||% attr(.df, "_xportr.df_metadata_")

  ## End of common section

  assert_data_frame(.df)
  assert_string(domain, null.ok = TRUE)
  assert_metadata(metadata)

  domain_name <- getOption("xportr.domain_name")
  format_name <- getOption("xportr.format_name")
  variable_name <- getOption("xportr.variable_name")

  if (inherits(metadata, "Metacore")) metadata <- metadata$var_spec

  if (domain_name %in% names(metadata) && !is.null(domain)) {
    metadata <- metadata %>%
      dplyr::filter(!!sym(domain_name) == domain & !is.na(!!sym(format_name)))
  } else {
    # Common check for multiple variables name
    check_multiple_var_specs(metadata, variable_name)
  }

  filtered_metadata <- metadata %>%
    filter(!!sym(variable_name) %in% names(.df))

  format <- filtered_metadata %>%
    select(!!sym(format_name)) %>%
    unlist() %>%
    toupper()

  names(format) <- filtered_metadata[[variable_name]]

  # vector of expected formats for clinical trials (usually character or date/time)
  # https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.5/leforinforref
  # /n0p2fmevfgj470n17h4k9f27qjag.htm#n0wi06aq4kydlxn1uqc0p6eygu75

  expected_formats <- c(
    NA,
    "",
    paste("$", 1:200, ".", sep = ""),
    paste("date", 5:11, ".", sep = ""),
    paste("time", 2:20, ".", sep = ""),
    paste("datetime", 7:40, ".", sep = ""),
    paste("yymmdd", 2:10, ".", sep = ""),
    paste("mmddyy", 2:10, ".", sep = ""),
    paste("ddmmyy", 2:10, ".", sep = ""),
    "E8601DA.",
    "E8601DA10.",
    "E8601DN.",
    "E8601DN10.",
    "E8601TM.",
    paste("E8601TM", 8:15, ".", sep = ""),
    paste("E8601TM", 8:15, ".", sort(rep(0:6, 8)), sep = ""),
    "E8601TZ.",
    paste("E8601TZ", 9:20, ".", sep = ""),
    paste("E8601TZ", 9:20, ".", sort(rep(0:6, 12)), sep = ""),
    "E8601TX.",
    paste("E8601TX", 9:20, ".", sep = ""),
    "E8601DT.",
    paste("E8601DT", 16:26, ".", sep = ""),
    paste("E8601DT", 16:26, ".", sort(rep(0:6, 11)), sep = ""),
    "E8601LX.",
    paste("E8601LX", 20:35, ".", sep = ""),
    "E8601LZ.",
    paste("E8601LZ", 9:20, ".", sep = ""),
    "E8601DX.",
    paste("E8601DX", 20:35, ".", sep = ""),
    "B8601DT.",
    paste("B8601DT", 15:26, ".", sep = ""),
    paste("B8601DT", 15:26, ".", sort(rep(0:6, 12)), sep = ""),
    "IS8601DA.",
    "B8601DA.",
    paste("B8601DA", 8:10, ".", sep = ""),
    "weekdate.",
    paste("weekdate", 3:37, ".", sep = ""),
    "mmddyy.",
    "ddmmyy.",
    "yymmdd.",
    "date.",
    "time.",
    "hhmm.",
    "IS8601TM.",
    "E8601TM.",
    "B8601TM."
  )

  # w.d format for numeric variables
  format_regex <- "^([1-9]|[12][0-9]|3[0-2])\\.$|^([1-9]|[12][0-9]|3[0-2])\\.([1-9]|[12][0-9]|3[0-1])$"


  for (i in seq_len(ncol(.df))) {
    format_sas <- purrr::pluck(format, colnames(.df)[i])
    if (is.na(format_sas) || is.null(format_sas)) {
      format_sas <- ""
    }
    # series of checks for formats

    # check that any variables ending DT, DTM, TM have a format
    if (grepl("DT$|DTM$|TM$", colnames(.df)[i]) == TRUE && format_sas == "") {
      message <- glue(
        "(xportr::xportr_format) {encode_vars(colnames(.df)[i])} is expected to have a format but does not."
      )
      xportr_logger(message, type = "warn")
    }

    # remaining checks to be carried out if a format exists
    if (format_sas != "") {
      # if the variable is character
      if (class(.df[[i]])[1] == "character") {
        # character variable formats should start with a $
        if (grepl("^\\$", format_sas) == FALSE) {
          message <- glue(
            "(xportr::xportr_format)",
            " {encode_vars(colnames(.df)[i])} is a character variable",
            " and should have a `$` prefix."
          )
          xportr_logger(message, type = "warn")
        }
        # character variable formats should have length <= 31 (excluding the $)
        if (nchar(gsub(".$", "", format_sas)) > 32) {
          message <- glue(
            "(xportr::xportr_format)",
            " Format for character variable {encode_vars(colnames(.df)[i])}",
            " should have length <= 31 (excluding `$`)."
          )
          xportr_logger(message, type = "warn")
        }
      }

      # if the variable is numeric
      if (class(.df[[i]])[1] == "numeric") {
        # numeric variables should not start with a $
        if (grepl("^\\$", format_sas) == TRUE) {
          message <- glue(
            "(xportr::xportr_format)",
            " {encode_vars(colnames(.df)[i])} is a numeric variable and",
            " should not have a `$` prefix."
          )
          xportr_logger(message, type = "warn")
        }
        # numeric variable formats should have length <= 32
        if (nchar(gsub(".$", "", format_sas)) > 32) {
          message <- glue(
            "(xportr::xportr_format)",
            " Format for numeric variable {encode_vars(colnames(.df)[i])}",
            " should have length <= 32."
          )
          xportr_logger(message, type = "warn")
        }
      }

      # check if the format is either one of the expected formats or follows the regular expression for w.d format
      if (
          !(format_sas %in% toupper(expected_formats)) &&
            (stringr::str_detect(format_sas, pattern = format_regex) == FALSE)) {
        message <- glue(
          "(xportr::xportr_format)",
          " Check format {encode_vars(format_sas)} for variable {encode_vars(colnames(.df)[i])}",
          " - is this correct?"
        )
        xportr_logger(message, type = "message")
      }
    }

    attr(.df[[i]], "format.sas") <- format_sas
  }

  .df
}

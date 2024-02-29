#' Extract Attribute From Data
#'
#' @param data Dataset to be exported as xpt file
#' @param attr SAS attributes such as label, format, type
#'
#' @return Character vector of attributes with column names assigned
#' @noRd
extract_attr <- function(data, attr = c("label", "format.sas")) {
  attr <- match.arg(attr)
  out <- lapply(data, function(.x) attr(.x, attr))
  out <- vapply(out,
    function(.x) ifelse(is.null(.x), "", .x),
    character(1L),
    USE.NAMES = FALSE
  )
  names(out) <- names(data)
  out
}

#' Assign Plural Grammar to Strings
#'
#' @param n Numeric value, usually the length of a character vector
#' @param msg1 Character value, usually a noun in singular form, such as Variable
#' @param msg2 Character value, usually a noun in plural form, such as Variables
#'
#' @return Singular or plural form of a word
#' @noRd
ntext <- function(n, msg1, msg2) {
  if (n == 1) msg1 else msg2
}

#' Assign Commas and Oxford Comma to a Series of Words in Text
#'
#' @param x Character Vector, usually a series of column names
#'
#' @return String of text where words are separated by commas and final
#' oxford comma ", and" convention
#' @noRd
fmt_comma <- function(x) {
  glue_collapse(x, sep = ", ", last = if (length(x) <= 2) " and " else ", and ")
}

#' Encode String of Variables in Tick Marks
#'
#' @param x Character Vector, of Variables
#'
#' @return String of text where series of words encased in ticks are separated by
#' commas and final oxford comma ", and" convention
#' @noRd
encode_vars <- function(x) {
  if (is.character(x)) {
    x <- encodeString(x, quote = "`")
  }

  fmt_comma(x)
}

#' Encode String of Values in Quotation Marks
#'
#' @param x Character Vector, of Values
#'
#' @return String of text where series of words encased in quotation marks are
#' separated by commas and final oxford comma ", and" convention
#' @noRd
encode_vals <- function(x) {
  if (is.character(x)) {
    x <- encodeString(x, quote = "'")
  }

  fmt_comma(x)
}

#' Variables Types Error Message Helper Function
#'
#' @param x Character vector of variable names
#'
#' @return String of text to append error message
#' @noRd
fmt_vars <- function(x) {
  vars <- ntext(length(x), "Variable", "Variables")
  glue("{vars} {encode_vars(x)}")
}

#' Variables Labels Error Message Helper Function
#'
#' @param x Character vector of variable labels
#'
#' @return String of text to append error message
#' @noRd
fmt_labs <- function(x) {
  labs <- ntext(length(x), "Label", "Labels")
  val <- paste0(names(x), "=", unname(x))
  glue("{labs} {encode_vals(val)}")
}

#' Variables Formats Error Message Helper Function
#'
#' @param x Character vector of variable formats
#'
#' @return String of text to append error message
#' @noRd
fmt_fmts <- function(x) {
  fmts <- ntext(length(x), "Format", "Formats")
  glue("{fmts} {encode_vals(x)}")
}

#' Check Variable Names Before Exporting to xpt
#'
#' @param varnames Column names of data
#'
#' @param list_vars_first Logical value to toggle where to list out column names
#' in error message
#'
#' @param err_cnd Character vector to initialize message
#'
#' @details Prior to exporting xpt file, check that column names meet appropriate
#' conditions like character limits, capitalization, and other naming conventions.
#'
#' @return An error message if incompatible variable names were used.
#' @noRd
xpt_validate_var_names <- function(varnames,
                                   list_vars_first = TRUE,
                                   err_cnd = character()) {
  # 1.1 Check length --
  chk_varlen <- varnames[nchar(varnames) > 8]

  if (length(chk_varlen) > 0) {
    err_cnd <- c(err_cnd, ifelse(list_vars_first,
      glue("{fmt_vars(chk_varlen)} must be 8 characters or less."),
      glue("
                      Must be 8 characters or less: {fmt_vars(chk_varlen)}.")
    ))
  }

  # 1.2 Check first character --
  chk_first_chr <- varnames[stringr::str_detect(
    stringr::str_sub(varnames, 1, 1),
    "[^[:alpha:]]"
  )]

  if (length(chk_first_chr) > 0) {
    err_cnd <- c(err_cnd, ifelse(list_vars_first,
      glue("{fmt_vars(chk_first_chr)} must start with a letter."),
      glue("
                      Must start with a letter: {fmt_vars(chk_first_chr)}.")
    ))
  }

  # 1.3 Check Non-ASCII and underscore characters --
  chk_alnum <- varnames[stringr::str_detect(varnames, "[^a-zA-Z0-9]")]

  if (length(chk_alnum) > 0) {
    err_cnd <- c(err_cnd, ifelse(list_vars_first,
      glue("{fmt_vars(chk_alnum)} cannot contain any non-ASCII, symbol or underscore characters."),
      glue("
                      Cannot contain any non-ASCII, symbol or underscore characters: {fmt_vars(chk_alnum)}.")
    ))
  }

  # 1.4 Check for any lowercase letters - or not all uppercase
  chk_lower <- varnames[!stringr::str_detect(
    stringr::str_replace_all(varnames, "[:digit:]", ""),
    "^[[:upper:]]+$"
  )]

  if (length(chk_lower) > 0) {
    err_cnd <- c(err_cnd, ifelse(list_vars_first,
      glue("{fmt_vars(chk_lower)} cannot contain any lowercase characters."),
      glue("
                      Cannot contain any lowercase characters {fmt_vars(chk_lower)}.")
    ))
  }
  return(err_cnd)
}

#' Internal list of formats to check
#' @noRd
.internal_format_list <- c(
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

#' Internal regex for format w.d
#' @noRd
.internal_format_regex <- paste(
  sep = "|",
  "^([1-9]|[12][0-9]|3[0-2])\\.$",
  "^([1-9]|[12][0-9]|3[0-2])\\.([1-9]|[12][0-9]|3[0-1])$"
)

#' Validate Dataset Can be Written to xpt
#'
#' Function used to validate dataframes before they are sent to
#' `haven::write_xpt` for writing.
#'
#' @param data Dataset to be exported as xpt file
#'
#' @return Returns a character vector of failed conditions
#'
#' @export
xpt_validate <- function(data) {
  assert_data_frame(data)

  err_cnd <- character()

  # 1.0 VARIABLES ----
  varnames <- names(data)
  err_cnd <- xpt_validate_var_names(varnames = varnames, err_cnd = err_cnd)


  # 2.0 LABELS ----
  labels <- extract_attr(data, attr = "label")

  # 2.1 Check length --
  chk_label_len <- labels[nchar(labels) > 40]

  if (length(chk_label_len) > 0) {
    err_cnd <- c(
      err_cnd,
      glue("{fmt_labs(chk_label_len)} must be 40 characters or less.")
    )
  }

  # 2.2 Check Non-ASCII and special characters
  chk_spl_chr <- labels[stringr::str_detect(labels, "[^[:ascii:]]")]

  if (length(chk_spl_chr) > 0) {
    err_cnd <- c(
      err_cnd,
      glue("{fmt_labs(chk_spl_chr)} cannot contain any non-ASCII, symbol or special characters.")
    )
  }


  # 3.0 Format Types ----
  formats <- extract_attr(data, attr = "format.sas")

  ## The usual expected formats in clinical trials: characters, dates
  # Formats: https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.5/leforinforref/n0zwce550r32van1fdd5yoixrk4d.htm
  expected_formats <- .internal_format_list

  format_regex <- .internal_format_regex

  # 3.1 Invalid types
  is_valid <- toupper(formats) %in% toupper(expected_formats) |
    purrr::map_lgl(formats, stringr::str_detect, format_regex)

  chk_formats <- formats[!is_valid]
  ## Remove the correctly numerically formatted variables
  if (length(chk_formats) > 0) {
    err_cnd <- c(
      err_cnd,
      glue("{fmt_fmts(names(chk_formats))} must have a valid format.")
    )
  }

  # 4.0 max length of Character variables <= 200 bytes
  max_nchar <- data %>%
    summarize(across(where(is.character), ~ max(0L, nchar(., type = "bytes"), na.rm = TRUE)))
  nchar_gt_200 <- max_nchar[which(max_nchar > 200)]
  if (length(nchar_gt_200) > 0) {
    err_cnd <- c(
      err_cnd,
      glue("Length of {names(nchar_gt_200)} must be 200 bytes or less.")
    )
  }

  return(err_cnd)
}

#' Get Origin Object of a Series of Pipes
#'
#' @return The R Object at the top of a pipe stack
#' @noRd
get_pipe_call <- function() {
  call_strs <- map(as.list(sys.calls()), ~ deparse1(.x, nlines = 1))
  top_call <- max(which(str_detect(call_strs, "%>%")))
  call_str <- call_strs[[top_call]]
  trimws(strsplit(call_str, "%>%", fixed = TRUE)[[1]][[1]])
}

#' Helper function to get the first class attribute
#'
#' @param x Any vector
#'
#' @return "character" or class of vector
#' @noRd
first_class <- function(x) {
  character_types <- getOption("xportr.character_types")
  class_ <- tolower(class(x)[1])
  if (class_ %in% character_types) {
    "character"
  } else {
    class_
  }
}

#' Check for multiple var name specs
#'
#' Detects cases where the domain name is not correctly defined and the full
#' specification is used.
#' This can lead to multiple warnings for the same variable. For instance, in
#' the FDA pilot 3 submission the column has variable name with uppercase
#' `Variable`, where the defaults for xportr is for lowercase `variable`.
#'
#' @param metadata  A data frame containing variable level metadata.
#' @param variable_name string with `getOption('xportr.variable_name')`
#' @noRd
check_multiple_var_specs <- function(metadata,
                                     variable_name = getOption("xportr.variable_name")) {
  variable_len <- pluck(metadata, variable_name) %||% c()
  if (NROW(variable_len) != NROW(unique(variable_len))) {
    cli_alert_info(
      glue(
        .sep = " ",
        "There are multiple specs for the same variable name.",
        "Check the metadata and variable name option",
        "`getOption('xportr.variable_name')`"
      )
    )
  }
}


#' Calculate the maximum length of variables
#'
#' Function to calculate the maximum length of variables in a given dataframe
#'
#' @inheritParams xportr_length
#'
#' @return Returns a dataframe with variables and their maximum length
#'
#' @noRd
variable_max_length <- function(.df) {
  assert_data_frame(.df)

  variable_length <- getOption("xportr.length")
  variable_name <- getOption("xportr.variable_name")

  max_nchar <- .df %>%
    summarize(across(where(is.character), ~ max(0L, nchar(., type = "bytes"), na.rm = TRUE)))


  xport_max_length <- data.frame()
  col <- 0
  for (var in names(.df)) {
    col <- col + 1

    xport_max_length[col, variable_name] <- var

    if (is.character(.df[[var]])) {
      xport_max_length[col, variable_length] <- max_nchar[var]
    } else {
      xport_max_length[col, variable_length] <- 8
    }
  }

  return(xport_max_length)
}

#' Custom check for metadata object
#'
#' Improvement on the message clarity over the default assert(...) messages.
#' @noRd
#' @param metadata A data frame or `Metacore` object containing variable level
#' @inheritParams checkmate::check_logical
#' metadata.
check_metadata <- function(metadata, include_fun_message, null.ok = FALSE) { # nolint: object_name.
  if (is.null(metadata) && null.ok) {
    return(TRUE)
  }

  extra_string <- ", 'Metacore' or set via 'xportr_metadata()'"
  if (!include_fun_message) {
    extra_string <- " or 'Metacore'"
  }

  if (!inherits(metadata, "Metacore") && !test_data_frame(metadata)) {
    return(
      glue(
        "Must be of type 'data.frame'{extra_string},",
        " not `{paste(class(metadata), collapse = '/')}"
      )
    )
  }
  TRUE
}

#' Custom assertion for metadata object
#' @noRd
#' @param metadata A data frame or `Metacore` object containing variable level
#' @inheritParams checkmate::check_logical
#' metadata.
assert_metadata <- function(metadata,
                            include_fun_message = TRUE,
                            null.ok = FALSE, # nolint: object_name.
                            add = NULL,
                            .var.name = vname(metadata)) { # nolint: object_name.
  makeAssertion(
    metadata,
    check_metadata(metadata, include_fun_message, null.ok),
    var.name = .var.name,
    collection = add
  )
}

#' Internal choices for verbose option
#' @noRd
.internal_verbose_choices <- c("none", "warn", "message", "stop")

#' Internal function to check xpt file size
#' @noRd
check_xpt_size <- function(path) {
  fs <- file.size(path)

  fs_string <- c(
    "i" = paste0("xpt file size is: ", round(fs / 1e+9, 2)), " GB.",
    "x" = paste0(
      "XPT file sizes should not exceed 5G. It is",
      " recommended you call `xportr_split` to split the file into smaller files."
    )
  )

  if (fs > 5e+9) {
    cli_warn(fs_string, class = "xportr.xpt_size") # nocov
  }

  invisible(NULL)
}

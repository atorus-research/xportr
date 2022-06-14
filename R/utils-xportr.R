xpt_validate_var_names <- function(varnames,
                                   list_vars_first = TRUE,
                                   err_cnd = character()) {
  
  # 1.1 Check length --
  chk_varlen <- varnames[nchar(varnames) > 8]
  
  if (length(chk_varlen) > 0) {
    err_cnd <- c(err_cnd, ifelse(list_vars_first,
                 glue("{fmt_vars(chk_varlen)} must be 8 characters or less."),
                 glue("
                      Must be 8 characters or less: {fmt_vars(chk_varlen)}.")))
  }
  
  # 1.2 Check first character --
  chk_first_chr <- varnames[stringr::str_detect(stringr::str_sub(varnames, 1, 1),
                                                "[^[:alpha:]]")]
  
  if (length(chk_first_chr) > 0) {
    err_cnd <- c(err_cnd, ifelse(list_vars_first,
                 glue("{fmt_vars(chk_first_chr)} must start with a letter."),
                 glue("
                      Must start with a letter: {fmt_vars(chk_first_chr)}.")))
  }
  
  # 1.3 Check Non-ASCII and underscore characters --
  chk_alnum <- varnames[stringr::str_detect(varnames, "[^a-zA-Z0-9]")]
  
  if (length(chk_alnum) > 0) {
    err_cnd <- c(err_cnd, ifelse(list_vars_first,
                 glue("{fmt_vars(chk_alnum)} cannot contain any non-ASCII, symbol or underscore characters."),
                 glue("
                      Cannot contain any non-ASCII, symbol or underscore characters: {fmt_vars(chk_alnum)}.")))  
  }
  
  # 1.4 Check for any lowercase letters - or not all uppercase
  chk_lower <- varnames[!stringr::str_detect(
                  stringr::str_replace_all(varnames, "[:digit:]", ""), 
                  "^[[:upper:]]+$")]
  
  if (length(chk_lower) > 0) {
    err_cnd <- c(err_cnd, ifelse(list_vars_first,
                 glue("{fmt_vars(chk_lower)} cannot contain any lowercase characters."),
                 glue("
                      Cannot contain any lowercase characters {fmt_vars(chk_lower)}.")))  
  }
  return(err_cnd)
}



xpt_validate <- function(data) {
  
  err_cnd <- character()
  
  # 1.0 VARIABLES ----
  varnames <- names(data)
  err_cnd <- xpt_validate_var_names(varnames = varnames, err_cnd = err_cnd)
  
  
  # 2.0 LABELS ----
  labels <- extract_attr(data, attr = "label")
  
  # 2.1 Check length --
  chk_label_len <- labels[nchar(labels) > 40]
  
  if (length(chk_label_len) > 0) {
    err_cnd <- c(err_cnd, 
                 glue("{fmt_labs(chk_label_len)} must be 40 characters or less."))    
  }
  
  # 2.2 Check Non-ASCII and special characters
  chk_spl_chr <- labels[stringr::str_detect(labels, "[<>]|[^[:ascii:]]")]
  
  if (length(chk_spl_chr) > 0) {
    err_cnd <- c(err_cnd,
                 glue("{fmt_labs(chk_spl_chr)} cannot contain any non-ASCII, symbol or special characters."))
  }
  
  # 3.0 VARIABLE TYPES ----
  types <- tolower(extract_attr(data, attr = "SAStype"))
  expected_types <- c('', 'text', 'integer', 'float', 'datetime', 'date', 'time',
                      'partialdate', 'partialtime', 'partialdatetime',
                      'incompletedatetime', 'durationdatetime', 'intervaldatetime')
  
  # 3.1 Invalid types --
  chk_types <- types[which(!types %in% expected_types)]
  
  if (length(chk_types) > 0) {
    err_cnd <- c(err_cnd,
                 glue("{fmt_vars(names(types))} must have a valid type."))
  }
  
 }

extract_attr <- function(data, attr = c("label", "SASformat", "SAStype", "SASlength")) {
  attr <- match.arg(attr)
  out <- lapply(data, function(.x) attr(.x, attr))
  out <- vapply(out, 
                function(.x) ifelse(is.null(.x), "", .x), 
                character(1L), USE.NAMES = FALSE)
  names(out) <- names(data)  
  out
}

ntext <- function(n, msg1, msg2) {
  if (n == 1) msg1 else msg2
}

fmt_comma <- function(x) {
  glue_collapse(x, sep = ", ", last = if (length(x) <= 2) " and " else ", and ") 
}

encode_vars <- function(x) {
  if (is.character(x)) {
    x <- encodeString(x, quote = "`")
  }
  
  fmt_comma(x)  
}

encode_vals <- function(x) {
  if (is.character(x)) {
    x <- encodeString(x, quote = "'")
  }
  
  fmt_comma(x)
}

fmt_vars <- function(x) {
  vars <- ntext(length(x), "Variable", "Variables")
  glue("{vars} {encode_vars(x)}")
}

fmt_labs <- function(x) {
  labs <- ntext(length(x), "Label", "Labels")
  val <- paste0(names(x), "=", unname(x))
  glue("{labs} {encode_vals(val)}")
}

get_pipe_call <- function() {
  call_strs <- map_chr(sys.calls(), as_label)
  top_call <- min(which(str_detect(call_strs, "%>%")))
  call_str <- as_label(sys.calls()[[top_call]])
  trimws(strsplit(call_str, "%>%", fixed = TRUE)[[1]][[1]])
}

# Helper function to get the first class attribute
first_class <- function(x) {
  characterTypes <- getOption("xportr.character_types")
  class_ <- class(x)[1]
  if (class_ %in% characterTypes) "character"
  else class_
}

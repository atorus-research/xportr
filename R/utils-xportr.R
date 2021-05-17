xpt_validate <- function(data) {
  
  err_cnd <- character()
  
  # 1.0 VARIABLES ----
  varnames <- names(data)
  
  # 1.1 Check length --
  chk_varlen <- varnames[nchar(varnames) > 8]
  
  if (length(chk_varlen) > 0) {
    err_cnd <- c(err_cnd, 
                 glue("{fmt_vars(chk_varlen)} must be 8 characters or less."))
  }

  # 1.2 Check first character --
  chk_first_chr <- varnames[stringr::str_detect(stringr::str_sub(varnames, 1, 1),
                                                "[^[:alpha:]]")]
  
  if (length(chk_first_chr) > 0) {
    err_cnd <- c(err_cnd, 
                 glue("{fmt_vars(chk_first_chr)} must start with a letter.")) 
  }

  # 1.3 Check Non-ASCII and underscore characters --
  chk_alnum <- varnames[stringr::str_detect(varnames, "[^a-zA-Z0-9]")]
  
  if (length(chk_alnum) > 0) {
    err_cnd <- c(err_cnd,
                 glue("{fmt_vars(chk_alnum)} cannot contain any non-ASCII, symbol or underscore characters."))  
  }
  
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
  
  # 3.2 Character datetime types -- 
  chk_datetime <- types[which(toupper(stringr::str_sub(names(types), start = -3L)) == "DTC")]
  
  if (length(chk_datetime) > 0) {
    err_cnd <- c(err_cnd,
                 glue("{fmt_vars(names(types))} must have a datetime related type."))    
  }
  
  err_cnd
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
  call_str <- as_label(sys.call(sys.parent() - 1L))
  trimws(strsplit(call_str, "%>%", fixed = TRUE)[[1]][[1]])
}

# Helper function to get the first class attribute
first_class <- function(x) {
  characterTypes <- getOption("xportr.character_types")
  class_ <- class(x)[1]
  if(class_ %in% characterTypes) "character"
  else class_
}

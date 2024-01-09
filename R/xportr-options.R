#' Get or set Xportr options
#'
#' @description
#'
#' There are two mechanisms for working with options for xportr. One is the
#' [options()] function, which is part of base R, and the other is the
#' `xportr_options()` function, which is in the xportr package. The reason for
#' these two mechanisms is has to do with legacy code and scoping.
#'
#' The [options()] function sets options globally, for the duration of the R
#' process. The [getOption()] function retrieves the value of an option. All
#' xportr related options of this type are prefixed with `"xportr."`.
#'
#'
#' @section Options with `options()`:
#'
#' \describe{
#' \item{xportr.df_domain_name (defaults to `"dataset"`)}{Description about this option ...}
#' \item{xportr.df_label (defaults to `"label"`)}{Description about this option ...}
#' \item{xportr.domain_name (defaults to `"dataset"`)}{Description about this option ...}
#' \item{xportr.variable_name (defaults to `"variable"`)}{Description about this option ...}
#' \item{xportr.type_name (defaults to `"type"`)}{Description about this option ...}
#' \item{xportr.label (defaults to `"label"`)}{Description about this option ...}
#' \item{xportr.length (defaults to `"length"`)}{Description about this option ...}
#' \item{xportr.format_name (defaults to `"format"`)}{Description about this option ...}
#' \item{xportr.format_verbose (defaults to `"none"`)}{Description about this option ...}
#' \item{xportr.label_verbose (defaults to `"none"`)}{Description about this option ...}
#' \item{xportr.length_verbose (defaults to `"none"`)}{Description about this option ...}
#' \item{xportr.type_verbose (defaults to `"label"`)}{Description about this option ...}
#' \item{xportr.character_types (defaults to `c("character", "char", "text", "date", "posixct", "posixt", "datetime", "time", "partialdate", "partialtime", "partialdatetime", "incompletedatetime", "durationdatetime", "intervaldatetime")`)}{Description about this option ...} # nolint
#' \item{xportr.numeric_types (defaults to `c("integer", "numeric", "num", "float")`)}{Description about this option ...} # nolint
#' \item{xportr.order_name (defaults to `"order"`)}{Description about this option ...}
#'
#'
#' @section Options with `xportr_options()`:
#'
#' There are a number of global options that affect xportr's behavior. These
#' can be set globally with `options()` or  with `xportr_options()`.
#' The `xportr_options()` function also returns the current options when nothing is passed to it.
#'
#' @param ... Options to set, with the form `name = value`.
#'
#' @examples
#' xportr_options(xportr.df_label = "data_label", xportr.label = "custom_label")
#' xportr_options("xportr.df_label")
#' xportr_options(c("xportr.label", "xportr.df_label"))
#' xportr_options()
#' @export
xportr_options <- function(...) {
  checkmate::assert_subset(names(list(...)), names(xportr_options))
  if (is.null(names(list(...)))) {
    if (length(list(...)) == 0) {
      queried_options <- names(xportr_options)
    } else {
      queried_options <- intersect(unlist(...), names(xportr_options))
    }
    current_options <- lapply(queried_options, function(opt) {
      getOption(opt)
    })
    names(current_options) <- queried_options
    return(current_options)
  }
  if (length(list(...)) > 0) {
    options_list <- list(...)
    xportr_options <- grep("^xportr\\.", names(options_list), value = TRUE)
    for (opt in xportr_options) {
      option_value <- options_list[[opt]]
      do.call(options, setNames(list(option_value), opt))
    }
  }
}

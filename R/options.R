#' Get or set xportr options
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
#' \item{xportr.df_domain_name}{defaults to `"dataset"`}:
#'  The name of the domain "name" column in dataset metadata.
#' \item{xportr.df_label}{defaults to `"label"`}:
#'  The column noting the dataset label in dataset metadata.
#' \item{xportr.domain_name}{defaults to `"dataset"`}:
#'  The name of the domain "name" column in variable metadata.
#' \item{xportr.variable_name}{defaults to `"variable"`}:
#'  The name of the variable "name" in variable metadata.
#' \item{xportr.type_name}{defaults to `"type"`}:
#'  The name of the variable type column in variable metadata.
#' \item{xportr.label}{defaults to `"label"`}:
#'  The name of the variable label column in variable metadata.
#' \item{xportr.length}{defaults to `"length"`}:
#'  The name of the variable length column in variable metadata.
#' \item{xportr.order_name}{defaults to `"order"`}:
#'  The name of the variable order column in variable metadata.
#' \item{xportr.format_name}{defaults to `"format"`}:
#'  The name of the variable format column in variable metadata.
#' \item{xportr.format_verbose}{defaults to `"none"`}:
#'  The default argument for the 'verbose' argument for `xportr_format`.
#' \item{xportr.label_verbose}{defaults to `"none"`}:
#'  The default argument for the 'verbose' argument for `xportr_label`.
#' \item{xportr.length_verbose}{defaults to `"none"`}:
#'  The default argument for the 'verbose' argument for `xportr_length`.
#' \item{xportr.type_verbose}{defaults to `"label"`}:
#'  The default argument for the 'verbose' argument for `xportr_type`.
#' \item{xportr.character_types}{defaults to `"character"`}:
#'  The default character vector used to explicitly coerce R classes to character XPT types.
#' \item{xportr.character_metadata_types}{defaults to `c("character", "char", "text", "date", "posixct", "posixt",
#'                                              "datetime", "time", "partialdate", "partialtime", "partialdatetime",
#'                                              "incompletedatetime", "durationdatetime", "intervaldatetime")`}:
#'  The default character vector used to explicitly coerce R classes to character XPT types.
#' \item{xportr.numeric_metadata_types}{defaults to `c("integer", "numeric", "num", "float")`}:
#'  The default character vector used to explicitly coerce R classes to numeric XPT types.
#' \item{xportr.numeric_types}{defaults to `c("integer", "float", "numeric", "posixct", "posixt", "time", "date")`}:
#'  The default character vector used to explicitly coerce R classes to numeric XPT types.
#' }
#'
#' @section Options with `xportr_options()`:
#'
#' Alternative to the `options()`, the `xportr_options()` function can be used to set the options.
#' The `xportr_options()` function also returns the current options when a character vector of
#' the options keys are passed into it. If nothing is passed into it, it returns the state of all xportr options.
#'
#' @param ... Options to set, with the form `name = value` or a character vector of option names.
#'
#' @examples
#' xportr_options("xportr.df_label")
#' xportr_options(xportr.df_label = "data_label", xportr.label = "custom_label")
#' xportr_options(c("xportr.label", "xportr.df_label"))
#' xportr_options()
#' @export
xportr_options <- function(...) {
  checkmate::assert_subset(names(list(...)), names(xportr_options_list))
  if (is.null(names(list(...)))) {
    if (length(list(...)) == 0) {
      queried_options <- names(xportr_options_list)
    } else {
      queried_options <- intersect(c(...), names(xportr_options_list))
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
      do.call(options, stats::setNames(list(option_value), opt))
    }
  }
}

#' A list with all the supported options of xportr
#'
#' An internal list with all the supported options of xportr with defaults
#' @keywords internal
xportr_options_list <- list(
  xportr.df_domain_name = getOption("xportr.df_domain_name", "dataset"),
  xportr.df_label = getOption("xportr.df_label", "label"),
  xportr.domain_name = getOption("xportr.domain_name", "dataset"),
  xportr.variable_name = getOption("xportr.variable_name", "variable"),
  xportr.type_name = getOption("xportr.type_name", "type"),
  xportr.label = getOption("xportr.label", "label"),
  xportr.length = getOption("xportr.length", "length"),
  xportr.order_name = getOption("xportr.order_name", "order"),
  xportr.format_name = getOption("xportr.format_name", "format"),
  xportr.format_verbose = getOption("xportr.format_verbose", "none"),
  xportr.label_verbose = getOption("xportr.label_verbose", "none"),
  xportr.length_verbose = getOption("xportr.length_verbose", "none"),
  xportr.type_verbose = getOption("xportr.type_verbose", "none"),
  xportr.character_types = getOption("xportr.character_types", "character"),
  xportr.character_metadata_types = getOption(
    "xportr.character_metadata_types",
    c(
      "character", "char", "text", "date", "posixct",
      "posixt", "datetime", "time", "partialdate",
      "partialtime", "partialdatetime",
      "incompletedatetime", "durationdatetime",
      "intervaldatetime"
    )
  ),
  xportr.numeric_metadata_types = getOption(
    "xportr.numeric_metadata_types",
    c("integer", "numeric", "num", "float")
  ),
  xportr.numeric_types = getOption(
    "xportr.numeric_types",
    c("integer", "float", "numeric", "posixct", "posixt", "time", "date", "hms")
  )
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(xportr_options_list) %in% names(op))
  if (any(toset)) options(xportr_options_list[toset])

  invisible()
}

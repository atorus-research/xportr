.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    xportr.df_domain_name = "dataset",
    xportr.df_label = "label",
    xportr.domain_name = "dataset",
    xportr.variable_name = "variable",
    xportr.type_name = "type",
    xportr.label = "label",
    xportr.length = "length",
    xportr.format_name = "format",
    xportr.format_verbose = "none",
    xportr.label_verbose = "none",
    xportr.length_verbose = "none",
    xportr.type_verbose = "none",
    xportr.character_types = c(
      "character", "char", "text", "date", "posixct",
      "posixt", "datetime", "time", "partialdate",
      "partialtime", "partialdatetime",
      "incompletedatetime", "durationdatetime",
      "intervaldatetime"
    ),
    xportr.numeric_types = c("integer", "numeric", "num", "float"),
    xportr.order_name = "order"
  )
  toset <- !(names(op.devtools) %in% names(op))
  if (any(toset)) options(op.devtools[toset])

  invisible()
}

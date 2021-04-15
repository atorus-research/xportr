.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    xportr.df_domain_name = "dataset",
    xportr.df_label = "label",
    xportr.coerse = "none",
    xportr.domain_name = "dataset",
    xportr.variable_name = "variable",
    xportr.type_name = "type",
    xportr.label = "label",
    xportr.length = "length",
    xportr.format_name = "format"
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])
  
  invisible()
}
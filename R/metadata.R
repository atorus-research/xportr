#' Set variable specifications and domain
#'
#' @param .df An R object with columns that can be coerced
#' @param metacore Either a data.frame that has the names of all possible columns
#'   and their types, or a `Metacore` object from the `Metacore` package. Required
#'   column names are dataset, variables, type
#' @param domain Name of the dataset. Ex ADAE/DM. This will be used to subset
#'   the metacore object. If none is passed it is assumed to be the name of the
#'   dataset passed in `.df`.
#'
#' @return `.df` dataset with metadata and domain attributes set
#' @export
#'
#' @examples
#' metadata <- data.frame(
#'   dataset = "test",
#'   variable = c("Subj", "Param", "Val", "NotUsed"),
#'   type = c("numeric", "character", "numeric", "character")
#' )
#'
#' adlb <- data.frame(
#'   Subj = as.character(123, 456, 789),
#'   Different = c("a", "b", "c"),
#'   Val = c("1", "2", "3"),
#'   Param = c("param1", "param2", "param3")
#' )
#'
#' xportr_metadata(adlb, metadata, "test")
xportr_metadata <- function(.df, metacore, domain = NULL) {
  ## Common section to detect domain from argument or pipes

  df_arg <- tryCatch(as_name(enexpr(.df)), error = function(err) NULL)
  domain <- get_domain(.df, df_arg, domain)
  if (!is.null(domain)) attr(.df, "_xportr.df_arg_") <- domain

  ## End of common section

  structure(.df, metadata = metacore)
}

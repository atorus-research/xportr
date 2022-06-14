#' Coerce variable type
#'
#' Current assumptions:
#' columns_meta is a data.frame with names "Variables", "Type"
#'
#' @param .df An R object with columns that can be coerced
#' @param metacore Either a data.frame that has the names of all possible columns
#'   and their types, or a `Metacore` object from the `Metacore` package. Required
#'   column names are dataset, variables, type
#' @param domain Name of the dataset. Ex ADAE/DM. This will be used to subset
#'   the metacore object. If none is passed it is assumed to be the name of the
#'   dataset passed in `.df`.
#' @param verbose The action the function takes when a variable isn't typed
#'   properly. Options are 'stop', 'warn', 'message', and 'none'
#'
#' @return Returns the modified table.
#' @export
#'
#' @examples
#' metacore <- data.frame(
#'   dataset = "test",
#'   variable = c("Subj", "Param", "Val", "NotUsed"),
#'   type = c("numeric", "character", "numeric", "character")
#' )
#'
#' .df <- data.frame(
#'  Subj = as.character(123, 456, 789),
#'  Different = c("a", "b", "c"),
#'  Val = c("1", "2", "3"),
#'  Param = c("param1", "param2", "param3")
#' )
#'
#' df2 <- xportr_type(.df, metacore, "test")
xportr_type <- function(.df, metacore, domain = NULL,
                        verbose = getOption('xportr.type_verbose', 'none')){
  
  # Name of the columns for working with metadata
  domain_name <- getOption("xportr.domain_name")
  variable_name <- getOption("xportr.variable_name")
  type_name <- getOption("xportr.type_name")
  characterTypes <- getOption("xportr.character_types")
  
  if (!is.null(domain) && !is.character(domain)) {
    abort(c("`domain` must be a vector with type <character>.",
            x = glue("Instead, it has type <{typeof(domain)}>."))
    )
  }
  
  df_arg <- as_name(enexpr(.df))
  
  if (!is.null(attr(.df, "_xportr.df_arg_"))) df_arg <- attr(.df, "_xportr.df_arg_")
  else if (identical(df_arg, ".")) {
    attr(.df, "_xportr.df_arg_") <- get_pipe_call()
    df_arg <- attr(.df, "_xportr.df_arg_") 
  }
  
  domain <- domain %||% df_arg
  
  if (!is.null(domain)) attr(.df, "_xportr.df_arg_") <- domain
  
  ## Pull out correct metadata
  if ("Metacore" %in% class(metacore)) metacore <- metacore$var_spec
  
  if (domain_name %in% names(metacore)) {
    metacore <- metacore %>%
      filter(!!sym(domain_name) == domain)
  }
  metacore <- metacore %>%
    select(!!sym(variable_name), !!sym(type_name))
  
  # Current class of table variables
  table_cols_types <- map(.df, first_class)
  
  # Produces a data.frame with Variables, Type.x(Table), and Type.y(metadata)
  meta_ordered <- left_join(
    data.frame(variable = names(.df), type = unlist(table_cols_types)),
    metacore,
    by = "variable"
  )
  
  # It is possible that a variable exists in the table that isn't in the metadata
  # it will be silently ignored here. This may happen depending on what a user
  # passes and the options they choose. The check_core function is the place
  # where this should be caught.
  type_mismatch_ind <- which(meta_ordered$type.x != meta_ordered$type.y)
  type_log(meta_ordered, type_mismatch_ind, verbose)
  
  
  # Check if variable types match
  is_correct <- sapply(meta_ordered[["type.x"]] == meta_ordered[["type.y"]], isTRUE)
  # Use the original variable iff metadata is missing that variable
  correct_type <- ifelse(is.na(meta_ordered[["type.y"]]), meta_ordered[["type.x"]], meta_ordered[["type.y"]])
  
  # Walk along the columns and coerce the variables. Modifying the columns
  # Directly instead of something like map_dfc to preserve any attributes.
  walk2(correct_type, seq_along(correct_type),
        function(x, i, is_correct) {
          if (!is_correct[i]) {
            if (correct_type[i] %in% characterTypes)
              .df[[i]] <<- as.character(.df[[i]])
            else .df[[i]] <<- as.numeric(.df[[i]])
          }
        }, is_correct)
  
  .df
}

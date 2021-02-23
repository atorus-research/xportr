#' Coerce variable type
#'
#' Current assumptions:
#' columns_meta is a data.frame with names "Variables", "Type"
#'
#' @param .df An R object with columns that can be coerced
#' @param datadef Either a data.frame that has the names of all possible columns
#'   and their types, or a `DataDef` object from the `DataDef` package. Required
#'   column names are dataset, variables, type
#' @param domain Name of the dataset. Ex ADAE/DM. This will be used to subset
#'   the datadef object. If none is passed it is assumed to be the name of the
#'   dataset passed in `.df`.
#' @param verbose The action the function takes when a variable isn't typed
#'   properly. Options are 'stop', 'warn', 'message', and 'none'
#'
#' @return Returns the modified table.
#' @export
#'
#' @examples
#' datadef <- data.frame(
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
#' df2 <- xportr_type(.df, datadef, "test")
xportr_type <- function(.df, datadef, domain = NULL,
                        verbose = getOption('xportr.coerse', 'none')){
  
  if(is.null(domain)) domain <- as_name(enexpr(.df))
  
  if("DataDef" %in% class(datadef)) {
    datadef <- datadef$ds_vars
  } else {
    datadef <- datadef %>%
      filter(dataset == domain) %>%
      select(variable, type)
  }
  
  # Current class of table variables
  table_cols_types <- map(.df, first_class)
  
  # Produces a data.frame with Variables, Type.x(Table), and Type.y(metadata)
  meta_ordered <- left_join(
    data.frame(variable = names(.df), type = unlist(table_cols_types)),
    datadef,
    by = "variable"
  )
  
  # It is possible that a variable exists in the table that isn't in the metadata
  # it will be silently ignored here. This may happen depending on what a user
  # passes and the options they choose. The check_core function is the place
  # where this should be caught.
  type_mismatch_ind <- which(meta_ordered$type.x != meta_ordered$type.y)
  if(length(type_mismatch_ind) > 0) {
    
    message <- glue(
      "Your data types do not match the specified data. They will be coerced\n",
      "Variable(Table)[Metadata]: \n",
      paste0(glue("{meta_ordered[type_mismatch_ind, 'variable']}",
                  "({meta_ordered[type_mismatch_ind, 'type.x']})",
                  "[{meta_ordered[type_mismatch_ind, 'type.y']}]"),
             collapse = "", sep = "\n")
    )
    
    if(verbose == "stop") abort(message)
    else if (verbose == "warn") warn(message)
    else if (verbose == "message") cli_alert_info(message)
  }
  
  
  # Variable coercion
  is_correct <- sapply(meta_ordered[["type.x"]] == meta_ordered[["type.y"]], isTRUE)
  correct_type <- ifelse(is.na(meta_ordered[["type.y"]]), meta_ordered[["type.x"]], meta_ordered[["type.y"]])
  
  # Walk along the columns and coerce the variables.
  walk2(correct_type, seq_along(correct_type),
        function(x, i, is_correct) {
          if(!is_correct[i]) {
            if(correct_type[i] %in% c("character", "Char"))
              .df[[i]] <<- as.character(.df[[i]])
            else .df[[i]] <<- as.numeric(.df[[i]])
          }
        }, is_correct)
  
  .df
}

# Helper function to get the first class attribute
first_class <- function(x) {
  class(x)[1]
}

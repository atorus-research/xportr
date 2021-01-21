#' Coerce variabe type
#'
#' Current assumptions:
#' columns_meta is a data.frame with names "Variables", "Type"
#'
#'test
#'
#' @param tab An R object with columns that can be coerced
#' @param columns_meta A data.frame that has the names of all possible columns
#'   and their types.
#' @param verbose The action the function takes when a variable isn't typed
#'   properly. Options are 'stop', 'warn', 'message', and 'none'
#'
#' @return Returns the modified table.
#' @noRd
#'
#' @examples
#' meta_example <- data.frame(
#'   Variable = c("Subj", "Param", "Val", "NotUsed"),
#'   Type = c("numeric", "character", "numeric", "character")
#' )
#'
#' df <- data.frame(
#'  Subj = as.character(123, 456, 789),
#'  Different = c("a", "b", "c"),
#'  Val = c("1", "2", "3"),
#'  Param = c("param1", "param2", "param3")
#' )
xpt_coerce_variable_type <- function(table, columns_meta,
                                     verbose = getOption('xportr.coerse', 'none')) {

  # Current class of table variables
  table_cols_types <- lapply(table, class)

  # Produces a data.frame with Variables, Type.x(Table), and Type.y(metadata)
  meta_ordered <- left_join(
    data.frame(Variable = names(table), Type = unlist(table_cols_types)),
    columns_meta,
    by = "Variable"
    )

  # It is possible that a variable exists in the table that isn't in the metadata
  # it will be silently ignored here. This may happen depending on what a user
  # passes and the options they choose. The check_core function is the place
  # where this shoudl be caught.
  type_mismatch_ind <- which(meta_ordered$Type.x != meta_ordered$Type.y)
  if(length(type_mismatch_ind) > 0) {
    if(verbose == "stop") {
      stop(paste0(
        "Your data types do not match the specified data. \n",
        "Variable(Table)[Metadata]: \n",
        paste0(meta_ordered[type_mismatch_ind, "Variable"], "(",
               meta_ordered[type_mismatch_ind, "Type.x"], ")", "[",
               meta_ordered[type_mismatch_ind, "Type.y"], "]", sep = "\n", collapse = "")
      ))
    } else if (verbose == "warn") {
      warning(paste0(
        "Your data types do not match the specified data. They will be coersed\n",
        "Variable(Table)[Metadata]: \n",
        paste0(meta_ordered[type_mismatch_ind, "Variable"], "(",
               meta_ordered[type_mismatch_ind, "Type.x"], ")", "[",
               meta_ordered[type_mismatch_ind, "Type.y"], "]", sep = "\n", collapse = "")
      ))
    } else if (verbose == "message") {
      message(paste0(
        "Your data types do not match the specified data. \n",
        "Variable(Table)[Metadata]: \n",
        paste0(meta_ordered[type_mismatch_ind, "Variable"], "(",
               meta_ordered[type_mismatch_ind, "Type.x"], ")", "[",
               meta_ordered[type_mismatch_ind, "Type.y"], "]", sep = "\n", collapse = "")
      ))
    }
  }


  # Variable coercion
  is_correct <- sapply(meta_ordered[["Type.x"]] == meta_ordered[["Type.y"]], isTRUE)
  correct_type <- ifelse(is.na(meta_ordered[["Type.y"]]), meta_ordered[["Type.x"]], meta_ordered[["Type.y"]])

  # Probably want to not use a for loop here but this is easy
  for(i in seq_along(correct_type)){
    if(!is_correct) {
      if(correct_type[i] == "character") table[[i]] <- as.character(table[[i]])
      else table[[i]] <- as.numeric(table[[i]])
    }
  }
  table
}

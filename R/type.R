#' Coerce variable type
#'
#' Current assumptions:
#' columns_meta is a data.frame with names "Variables", "Type"
#'
#' @param .df An R object with columns that can be coerced
#' @param datadef A data.frame that has the names of all possible columns
#'   and their types.
#' @param verbose The action the function takes when a variable isn't typed
#'   properly. Options are 'stop', 'warn', 'message', and 'none'
#'
#' @return Returns the modified table.
#' @noRd
#'
#' @examples
#' datadef <- data.frame(
#'   Variable = c("Subj", "Param", "Val", "NotUsed"),
#'   Type = c("numeric", "character", "numeric", "character")
#' )
#'
#' .df <- data.frame(
#'  Subj = as.character(123, 456, 789),
#'  Different = c("a", "b", "c"),
#'  Val = c("1", "2", "3"),
#'  Param = c("param1", "param2", "param3")
#' )
#'
#' df2 <- xportr_type(.df, datadef)
xportr_type <- function(.df, datadef,
                                     verbose = getOption('xportr.coerse', 'none')) {

  # Current class of table variables
  table_cols_types <- lapply(.df, class)

  # Produces a data.frame with Variables, Type.x(Table), and Type.y(metadata)
  meta_ordered <- left_join(
    data.frame(Variable = names(.df), Type = unlist(table_cols_types)),
    datadef,
    by = "Variable"
    )

  # It is possible that a variable exists in the table that isn't in the metadata
  # it will be silently ignored here. This may happen depending on what a user
  # passes and the options they choose. The check_core function is the place
  # where this should be caught.
  type_mismatch_ind <- which(meta_ordered$Type.x != meta_ordered$Type.y)
  if(length(type_mismatch_ind) > 0) {
    if(verbose == "stop") {
      stop(glue(
        "Your data types do not match the specified data. They will be coerced\n",
        "Variable(Table)[Metadata]: \n",
        paste0(glue("{meta_ordered[type_mismatch_ind, 'Variable']}",
                    "({meta_ordered[type_mismatch_ind, 'Type.x']})",
                    "[{meta_ordered[type_mismatch_ind, 'Type.y']}]"),
               collapse = "", sep = "\n")
      ))
    } else if (verbose == "warn") {
      warning(glue(
        "Your data types do not match the specified data. They will be coerced\n",
        "Variable(Table)[Metadata]: \n",
        paste0(glue("{meta_ordered[type_mismatch_ind, 'Variable']}",
             "({meta_ordered[type_mismatch_ind, 'Type.x']})",
             "[{meta_ordered[type_mismatch_ind, 'Type.y']}]"),
             collapse = "", sep = "\n")
        ))
    } else if (verbose == "message") {
      message(glue(
        "Your data types do not match the specified data. They will be coerced\n",
        "Variable(Table)[Metadata]: \n",
        paste0(glue("{meta_ordered[type_mismatch_ind, 'Variable']}",
                    "({meta_ordered[type_mismatch_ind, 'Type.x']})",
                    "[{meta_ordered[type_mismatch_ind, 'Type.y']}]"),
               collapse = "", sep = "\n")
      ))
    }
  }


  # Variable coercion
  is_correct <- sapply(meta_ordered[["Type.x"]] == meta_ordered[["Type.y"]], isTRUE)
  correct_type <- ifelse(is.na(meta_ordered[["Type.y"]]), meta_ordered[["Type.x"]], meta_ordered[["Type.y"]])

  walk2(correct_type, seq_along(correct_type),
             function(x, i, is_correct) {
               if(!is_correct[i]) {
                 if(correct_type[i] == "character") .df[[i]] <<- as.character(.df[[i]])
                 else .df[[i]] <<- as.numeric(.df[[i]])
               }
             }, is_correct)


  .df
}

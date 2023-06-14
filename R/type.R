#' Coerce variable type
#'
#' Current assumptions:
#' columns_meta is a data.frame with names "Variables", "Type"
#'
#' @param .df An R object with columns that can be coerced
#' @param metadata Either a data.frame that has the names of all possible columns
#'   and their types, or a `Metacore` object from the `Metacore` package. Required
#'   column names are dataset, variables, type
#' @param domain Name of the dataset. Ex ADAE/DM. This will be used to subset
#'   the metadata object. If none is passed it is assumed to be the name of the
#'   dataset passed in `.df`.
#' @param verbose The action the function takes when a variable isn't typed
#'   properly. Options are 'stop', 'warn', 'message', and 'none'
#' @param metacore `r lifecycle::badge("deprecated")` Previously used to pass metadata now renamed with `metadata`
#'
#' @return Returns the modified table.
#' @export
#'
#' @examples
#' metadata <- data.frame(
#'   dataset = "test",
#'   variable = c("Subj", "Param", "Val", "NotUsed"),
#'   type = c("numeric", "character", "numeric", "character"),
#'   format = NA
#' )
#'
#' .df <- data.frame(
#'   Subj = as.character(123, 456, 789),
#'   Different = c("a", "b", "c"),
#'   Val = c("1", "2", "3"),
#'   Param = c("param1", "param2", "param3")
#' )
#'
#' df2 <- xportr_type(.df, metadata, "test")
xportr_type <- function(.df,
                        metadata = NULL,
                        domain = NULL,
                        verbose = getOption("xportr.length_verbose", "none"),
                        metacore = deprecated()) {
  if (!missing(metacore)) {
    lifecycle::deprecate_warn(
      when = "0.3.0",
      what = "xportr_format(metacore = )",
      with = "xportr_format(metadata = )"
    )
    metadata <- metacore
  }
  # Name of the columns for working with metadata
  domain_name <- getOption("xportr.domain_name")
  variable_name <- getOption("xportr.variable_name")
  type_name <- getOption("xportr.type_name")
  characterTypes <- c(getOption("xportr.character_types"), "_character")
  numericTypes <- c(getOption("xportr.numeric_types"), "_numeric")
  format_name <- getOption("xportr.format_name")

  ## Common section to detect domain from argument or pipes

  df_arg <- tryCatch(as_name(enexpr(.df)), error = function(err) NULL)
  domain <- get_domain(.df, df_arg, domain)
  if (!is.null(domain)) attr(.df, "_xportr.df_arg_") <- domain

  ## End of common section

  ## Pull out correct metadata
  metadata <- metadata %||%
    attr(.df, "_xportr.df_metadata_") %||%
    rlang::abort("Metadata must be set with `metadata` or `xportr_metadata()`")

  if (inherits(metadata, "Metacore")) {
    metadata <- metadata$var_spec
  }

  if (domain_name %in% names(metadata)) {
    metadata <- metadata %>%
      filter(!!sym(domain_name) == domain)
  }

  metacore <- metadata %>%
    select(!!sym(variable_name), !!sym(type_name), !!sym(format_name))

  # Current class of table variables
  table_cols_types <- map(.df, first_class)

  # Produces a data.frame with Variables, Type.x(Table), and Type.y(metadata)
  meta_ordered <- left_join(
    data.frame(variable = names(.df), type = unlist(table_cols_types)),
    metadata,
    by = "variable"
  ) %>%
    mutate(
      # _character is used here as a mask of character, in case someone doesn't
      # want 'character' coerced to character
      type.x = if_else(type.x %in% characterTypes, "_character", type.x),
      type.x = if_else(type.x %in% numericTypes | (grepl("DT$|DTM$|TM$", variable) & !is.na(format)),
        "_numeric",
        type.x
      ),
      # type.x = if_else(grepl("DTC$", variable) & type.x == "_character", "Date", type.x),
      type.y = if_else(is.na(type.y), type.x, type.y),
      type.y = tolower(type.y),
      type.y = if_else(type.y %in% characterTypes | (grepl("DTC$", variable) & is.na(format)), "_character", type.y),
      type.y = if_else(type.y %in% numericTypes, "_numeric", type.y)
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
  walk2(
    correct_type, seq_along(correct_type),
    function(x, i, is_correct) {
      if (!is_correct[i]) {
        orig_attributes <- attributes(.df[[i]])
        orig_attributes$class <- NULL
        if (correct_type[i] %in% characterTypes) {
          .df[[i]] <<- as.character(.df[[i]])
        } else {
          .df[[i]] <<- as.numeric(.df[[i]])
        }
        attributes(.df[[i]]) <<- orig_attributes
      }
    }, is_correct
  )
  .df
}

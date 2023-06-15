#' Coerce variable type
#'
#' XPT v5 datasets only have data types of character and numeric. `xportr_type`
#' attempts to collapse R classes to those two XPT types. The
#' 'xportr.character_types' option is used to explicitly collapse the class of a
#' column to character using `as.character`. Similarly, 'xportr.numeric_types'
#' will collapse a column to a numeric type. If no type is passed for a
#' variable and it isn't identifed as a timing variable, it is assumed to be numeric and coerced with `as.numeric`.
#'
#' Certain care should be taken when using timing variables. R serializes dates
#' based on a reference date of 01/01/1970 where XPT uses 01/01/1960. This can
#' result in dates being 10 years off when outputting from R to XPT if you're
#' using a date class. For this reason, `xportr` will try to determine what
#' should happen with variables that appear to be used to denote time.
#'
#' For variables that end in DT, DTM, or, TM, if they are not explicitly noted
#' in 'xportr.numeric_types' or 'xportr.character_types', they are coerced to
#' numeric results.
#'
#' @inheritParams xportr_length
#'
#' @section Messaging: `type_log()` is the primary messaging tool for
#'   `xportr_type()`. The number of column types that mismatch the reported type
#'   in the metadata, if any, is reported by by `xportr_type()`. If there are any
#'   type mismatches, and the 'verbose' argument is 'stop', 'warn', or
#'   'message', each mismatch will be detailed with the actual type in the data
#'   and the type noted in the metadata.
#'
#' @section Metadata: The argument passed in the 'metadata' argument can either
#'   be a metacore object, or a data.frame containing the data listed below. If
#'   metacore is used, no changes to options are required.
#'
#'   For data.frame 'metadata' arguments four columns must be present:
#'
#'   1) Domain Name - passed as the 'xportr.domain_name' option. Default:
#'   "dataset". This is the column subset by the 'domain' argument in the
#'   function.
#'
#'   2) Format Name - passed as the 'xportr.format_name' option. Default:
#'   "format". Character values to update the 'format.sas' attribute of the
#'   column. This is passed to `haven::write` to note the format.
#'
#'   3) Variable Name - passed as the 'xportr.variable_name' option. Default:
#'   "variable". This is used to match columns in '.df' argument and the
#'   metadata.
#'
#'   4) Variable Type - passed as the 'xportr.type_name'. Default: "type". This
#'   is used to note the XPT variable "type" options are numeric or character.
#'
#'   5) (Option only) Character Types - The list of classes that should be
#'   explicitly coerced to a XPT Character type. Default: c( "character",
#'   "char", "text", "date", "posixct", "posixt", "datetime", "time",
#'   "partialdate", "partialtime", "partialdatetime", "incompletedatetime",
#'   "durationdatetime", "intervaldatetime")
#'
#'   6) (Option only) Numeric Types - The list of classes that should be
#'   explicitly coerced to a XPT numeric type. Default: c("integer", "numeric",
#'   "num", "float")
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
                        verbose = getOption("xportr.type_verbose", "none"),
                        metacore = deprecated()) {
  if (!missing(metacore)) {
    lifecycle::deprecate_warn(
      when = "0.3.0",
      what = "xportr_type(metacore = )",
      with = "xportr_type(metadata = )"
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

  # Common check for multiple variables name
  check_multiple_var_specs(metadata, variable_name)

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

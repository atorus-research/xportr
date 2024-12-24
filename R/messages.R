#' Utility Logging Function
#'
#' Functions to output user messages, usually relating to differences
#' found between dataframe and the metacore/metadata object
#'
#' @param message Output to be sent out for user
#' @param type Three types: abort, warn, inform
#' @param ... additional arguments if needed
#'
#' @return Output to Console
#' @noRd
xportr_logger <- function(message, type = "none", ...) {
  assert_character(message)
  assert_choice(type, choices = .internal_verbose_choices)

  log_fun <- switch(type,
    stop = abort,
    warn = warn,
    message = inform,
    return()
  )

  do.call(log_fun, list(message, ...))
}

#' Utility for Renaming Variables
#'
#' @param tidy_names_df dataframe
#' @param verbose Provides additional messaging for user
#'
#' @return Output to Console
#' @noRd
var_names_log <- function(tidy_names_df, verbose) {
  assert_data_frame(tidy_names_df)
  assert_choice(verbose, choices = .internal_verbose_choices)

  only_renames <- tidy_names_df %>%
    filter(original_varname != renamed_var) %>%
    mutate(
      renamed_msg = glue(
        "Var {col_pos} : '{original_varname}' was renamed to 'renamed_var'"
      )
    )

  # Message regarding number of variables that were renamed/ modified
  num_renamed <- nrow(only_renames)
  tot_num_vars <- nrow(tidy_names_df)

  cli_h2(glue(
    .sep = " ",
    "{num_renamed} of {tot_num_vars}",
    "({round(100*(num_renamed/tot_num_vars), 1)}%)",
    "variables were renamed"
  ))

  # Message stating any renamed variables each original variable and it's new name
  if (nrow(only_renames) > 0) {
    purrr::walk(only_renames$renamed_msg, ~ xportr_logger(.x, verbose))
  }

  # Message checking for duplicate variable names after renamed (Pretty sure
  # this is impossible) but good to have a check none-the-less.
  dups <- tidy_names_df %>% filter(renamed_n > 1)
  if (nrow(dups) != 0) {
    cli::cli_alert_danger(
      glue(
        .sep = " ",
        "Duplicate renamed term(s) were created.",
        "Consider creating dictionary terms for:",
        encode_vars(unique(dups$renamed_var))
      )
    )
  }
}

#' Utility for Types
#'
#' @param meta_ordered fill in later
#' @param type_mismatch_ind fill in later
#' @param verbose Provides additional messaging for user
#'
#' @return Output to Console
#' @noRd
type_log <- function(meta_ordered, type_mismatch_ind, verbose) {
  assert_data_frame(meta_ordered)
  assert_integer(type_mismatch_ind)
  assert_choice(verbose, choices = .internal_verbose_choices)

  if (length(type_mismatch_ind) > 0) {
    cli_h2("Variable type mismatches found.")
    cli_alert_success("{ length(type_mismatch_ind) } variable{?s} coerced")

    meta_mismatch <- meta_ordered[type_mismatch_ind, ]
    message <- glue(
      "Variable type(s) in dataframe don't match metadata: ",
      "{encode_vars(meta_ordered[type_mismatch_ind, 'variable'])}\n",
      paste0(
        "- `", meta_mismatch$variable, "` was coerced to ",
        ifelse(meta_mismatch$type.y == "_character", "<character>", "<numeric>"),
        ". (type in data: ", meta_mismatch$orig_type_data, ", type in metadata: ",
        meta_mismatch$orig_type_meta, ")",
        collapse = "\n"
      ),
      paste(
        "\ni Types in metadata considered as character (xportr.character_metadata_types option):",
        encode_vals(getOption("xportr.character_metadata_types")),
        collapse = " "
      ),
      paste(
        "\ni Types in metadata considered as numeric (xportr.numeric_metadata_types option):",
        encode_vals(getOption("xportr.numeric_metadata_types")),
        collapse = " "
      ),
      paste(
        "\ni Types in data considered as character (xportr.character_types option):",
        encode_vals(getOption("xportr.character_types")),
        collapse = " "
      ),
      paste(
        "\ni Types in data considered as numeric (xportr.numeric_types option):",
        encode_vals(getOption("xportr.numeric_types")),
        collapse = " "
      )
    )

    xportr_logger(message, verbose)
  }
}

#' Utility for Lengths
#'
#' @param miss_vars Variables missing from metadata
#' @param miss_length Variables with missing length in metadata
#' @param verbose Provides additional messaging for user
#'
#' @return Output to Console
#' @noRd
length_log <- function(miss_vars, miss_length, verbose) {
  assert_character(miss_vars)
  assert_character(miss_length)
  assert_choice(verbose, choices = .internal_verbose_choices)

  if (length(c(miss_vars, miss_length)) > 0) {
    cli_h2("Variable lengths missing from metadata.")
    cli_alert_success("{ length(c(miss_vars, miss_length)) } lengths resolved {encode_vars(c(miss_vars, miss_length))}")

    xportr_logger(
      glue(
        "Variable(s) present in dataframe but doesn't exist in `metadata`.",
        "Problem with {encode_vars(miss_vars)}"
      ),
      type = verbose
    )
  }
}

#' Utility for Variable Labels
#'
#' @param miss_vars Missing variables in metadata
#' @param verbose Provides additional messaging for user
#'
#' @return Output to Console
#' @noRd
label_log <- function(miss_vars, verbose) {
  assert_character(miss_vars)
  assert_choice(verbose, choices = .internal_verbose_choices)

  if (length(miss_vars) > 0) {
    cli_h2("Variable labels missing from metadata.")
    cli_alert_success("{ length(miss_vars) } labels skipped")

    xportr_logger(
      c("Variable(s) present in dataframe but doesn't exist in `metadata`.",
        x = glue("Problem with {encode_vars(miss_vars)}")
      ),
      type = verbose
    )
  }
}

#' Utility for Ordering
#'
#' @param reordered_vars Number of variables reordered
#' @param moved_vars Number of variables moved in the dataset
#' @param verbose Provides additional messaging for user
#'
#' @return Output to Console
#' @noRd
var_ord_msg <- function(reordered_vars, moved_vars, verbose) {
  assert_character(reordered_vars)
  assert_character(moved_vars)
  assert_choice(verbose, choices = .internal_verbose_choices)

  if (length(moved_vars) > 0) {
    cli_h2("{ length(moved_vars) } variables not in spec and moved to end")
    message <- glue(
      "Variable moved to end in `.df`: { encode_vars(moved_vars) }"
    )
    xportr_logger(message, verbose)
  } else {
    cli_h2("All variables in dataset are found in `metadata`")
  }

  if (length(reordered_vars) > 0) {
    cli_h2("{ length(reordered_vars) } reordered in dataset")
    message <- glue(
      "Variable reordered in `.df`: { encode_vars(reordered_vars) }"
    )
    xportr_logger(message, verbose)
  } else {
    cli_h2("All variables in dataset are ordered")
  }
}

#' Utility for data Lengths
#'
#' @param max_length Dataframe with data and metadata length
#' @param verbose Provides additional messaging for user
#'
#' @return Output to Console
#' @noRd
max_length_msg <- function(max_length, verbose) {
  assert_data_frame(max_length)
  assert_choice(verbose, choices = .internal_verbose_choices)

  if (nrow(max_length) > 0) {
    cli_h2("Variable length is shorter than the length specified in the metadata.")

    xportr_logger(
      glue(
        "Update length in metadata to trim the variables:"
      ),
      type = verbose
    )

    xportr_logger(
      glue(
        "{format(max_length[[1]], width = 8)} has a length of {format(as.character(max_length[[2]]), width = 3)}",
        " and a length of {format(as.character(max_length[[3]]), width = 3)} in metadata"
      ),
      type = verbose
    )
  }
}

#' Utility for Missing Domain
#'
#' @param domain Domain passed by user
#' @param domain_name Name of the domain column in metadata
#' @param verbose Provides additional messaging for user
#'
#' @return Output to Console
#' @noRd
log_no_domain <- function(domain, domain_name, verbose) {
  cli_h2("Domain not found in metadata.")
  xportr_logger(
    glue(
      "Domain '{domain}' not found in metadata '{domain_name}' column."
    ),
    type = verbose
  )
}

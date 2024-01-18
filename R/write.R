#' Write xpt v5 transport file
#'
#' Writes a local data frame into SAS transport file of version 5. The SAS
#' transport format is an open format, as is required for submission of the data
#' to the FDA.
#'
#' @param .df A data frame to write.
#' @param path Path where transport file will be written. File name sans will be
#'   used as `xpt` name.
#' @param label `r lifecycle::badge("deprecated")` Previously used to to set the Dataset label.
#' Use the `metadata` argument to set the dataset label.
#' @param strict_checks If TRUE, xpt validation will report errors and not write
#'   out the dataset. If FALSE, xpt validation will report warnings and continue
#'   with writing out the dataset. Defaults to FALSE
#' @inheritParams xportr_length
#'
#' @details
#'   * Variable and dataset labels are stored in the "label" attribute.
#'
#'   * SAS length are stored in the "SASlength" attribute.
#'
#'   * SAS format are stored in the "SASformat" attribute.
#'
#'   * SAS type are stored in the "SAStype" attribute.
#'
#' @return A data frame. `xportr_write()` returns the input data invisibly.
#' @export
#'
#' @examples
#' adsl <- data.frame(
#'   SUBL = as.character(123, 456, 789),
#'   DIFF = c("a", "b", "c"),
#'   VAL = c("1", "2", "3"),
#'   PARAM = c("param1", "param2", "param3")
#' )
#'
#' var_spec <- data.frame(
#'   dataset = "adsl",
#'   label = "Subject-Level Analysis Dataset",
#'   data_label = "ADSL"
#' )
#' xportr_write(adsl,
#'   path = paste0(tempdir(), "/adsl.xpt"),
#'   domain = "adsl",
#'   metadata = var_spec,
#'   strict_checks = FALSE
#' )
#'
xportr_write <- function(.df,
                         path,
                         metadata = NULL,
                         domain = attr(.df, "_xportr.df_arg_"),
                         strict_checks = FALSE,
                         label = deprecated()) {
  assert_data_frame(.df)
  assert_string(path)
  assert_metadata(metadata, null.ok = TRUE)
  assert_logical(strict_checks)

  if (!is.null(domain)) attr(.df, "_xportr.df_arg_") <- domain

  path <- normalizePath(path, mustWork = FALSE)

  name <- tools::file_path_sans_ext(basename(path))

  if (!missing(label)) {
    lifecycle::deprecate_warn(
      when = "0.3.2",
      what = "xportr_write(label = )",
      with = "xportr_write(metadata = )"
    )
    assert_string(label, null.ok = TRUE, max.chars = 40)
    metadata <- data.frame(dataset = domain, label = label)
  }
  if (!is.null(metadata)) {
    .df <- xportr_df_label(.df, metadata = metadata, domain = domain)
  }

  if (nchar(name) > 8) {
    assert(".df file name must be 8 characters or less.", .var.name = "path")
  }

  checks <- xpt_validate(.df)

  if (stringr::str_detect(name, "[^a-zA-Z0-9]")) {
    checks <- c(checks, "`.df` cannot contain any non-ASCII, symbol or underscore characters.")
  }

  if (length(checks) > 0) {
    if (!strict_checks) {
      warn(c("The following validation checks failed:", checks))
    } else {
      abort(c("The following validation checks failed:", checks))
    }
  }

  data <- as.data.frame(.df)

  tryCatch(
    write_xpt(data, path = path, version = 5, name = name),
    error = function(err) {
      rlang::abort(
        paste0(
          "Error reported by haven::write_xpt, error was: \n",
          err
        )
      )
    }
  )

  invisible(data)
}

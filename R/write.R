#' Write xpt v5 transport file
#'
#' Writes a local data frame into SAS transport file of version 5. The SAS
#' transport format is an open format, as is required for submission of the data
#' to the FDA.
#'
#' @param .df A data frame to write.
#' @param path Path where transport file will be written. File name sans will be
#'   used as `xpt` name.
#' @param label Dataset label. It must be <=40 characters.
#' @param strict_checks If TRUE, xpt validation will report errors and not write
#'   out the dataset. If FALSE, xpt validation will report warnings and continue
#'   with writing out the dataset. Defaults to FALSE
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
#'   Subj = as.character(123, 456, 789),
#'   Different = c("a", "b", "c"),
#'   Val = c("1", "2", "3"),
#'   Param = c("param1", "param2", "param3")
#' )
#'
#' xportr_write(adsl,
#'   path = paste0(tempdir(), "/adsl.xpt"),
#'   label = "Subject-Level Analysis",
#'   strict_checks = FALSE
#' )
#'
xportr_write <- function(.df, path, label = NULL, strict_checks = FALSE) {
  path <- normalizePath(path, mustWork = FALSE)

  name <- tools::file_path_sans_ext(basename(path))

  if (nchar(name) > 8) {
    abort("`.df` file name must be 8 characters or less.")
  }

  if (stringr::str_detect(name, "[^a-zA-Z0-9]")) {
    abort("`.df` cannot contain any non-ASCII, symbol or underscore characters.")
  }

  if (!is.null(label)) {
    if (nchar(label) > 40) {
      abort("`label` must be 40 characters or less.")
    }

    if (stringr::str_detect(label, "[^[:ascii:]]")) {
      abort("`label` cannot contain any non-ASCII, symbol or special characters.")
    }

    attr(.df, "label") <- label
  }

  checks <- xpt_validate(.df)

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

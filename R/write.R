#' Write SAS transport file
#'
#' Writes a local data frame into SAS transport file of version 5. The SAS
#' transport format is a open format, as is required for submission of the data
#' to the FDA.
#'
#' @param .df A data frame to write.
#' @param path Path to a file where the data will be written.
#' @param label Dataset label. It must be<=40 characters.
#'
#' @details
#'   * Variable and dataset labels are stored in the "label" attribute.
#'   * SAS length are stored in the "SASlength" attribute.
#'   * SAS format are stored in the "SASformat" attribute.
#'   * SAS type are stored in the "SAStype" attribute.
#'
#' @return A data frame. `write_xport()` returns the input data invisibly.
#' @export
#'
#' @examples
#' tmp <- tempfile(fileext = ".xpt")
#' xportr_write(mtcars, tmp, label = "Motor Trend Car Road Tests")
xportr_write <- function(.df, path, label = NULL) {

  data_call <- sym(enexpr(.df))
  name <- as_name(data_call)

  if (nchar(name) > 8) {
    abort("`.df` must be 8 characters or less.")
  }

  if (stringr::str_detect(name, "[^a-zA-Z0-9]")) {
    abort("`.df` cannot contain any non-ASCII, symbol or underscore characters.")
  }

  if (!is.null(label)) {

    if (nchar(label) > 40)
      abort("`label` must be 40 characters or less.")

    if (stringr::str_detect(label, "[<>]|[^[:ascii:]]"))
      abort("`label` cannot contain any non-ASCII, symbol or special characters.")

    attr(.df, "label") <- label
  }

  checks <- xpt_validate(.df)

  if (length(checks) > 0) {
    names(checks) <- rep("x", length(checks))
    abort(c("The following validation failed:", checks))
  }

  # `write.xport` supports only the class data.frame
  data <- as.data.frame(.df)

  exec(SASxport::write.xport,
              !! data_call := data,
              file = normalizePath(path, mustWork = FALSE),
              autogen.formats = FALSE)

  invisible(data)
}

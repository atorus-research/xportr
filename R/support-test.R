#' Custom expect function to test result of xportr_length
#'
#' @param result data.frame with `width` attribute on its columns.
#' @param metadata_length vector of numeric with expected lengths for each
#' column width.
#'
#' @return The first argument, invisibly.
#' @keywords internal
expect_attr_width <- function(result, metadata_length) {
  test_widths <- map(
    colnames(result), ~ attributes(result[[.x]]) %>% pluck("width")
  ) %>%
    unlist() == metadata_length

  test_widths %>%
    all() %>%
    testthat::expect_true()
  invisible(result)
}


#' Minimal data frame mock of a valid ADaM dataset
#'
#' This function is only used in tests.
#'
#' @param n_rows Numeric value that indicates the number of rows of the data
#' frame
#' @param cols Vector of characters that indicates which columns to return.
#' By default only `x` and `y` are returned with numeric contents.
#'
#' @return A data.frame mimicking a valid ADaM dataset.
#' @keywords internal
minimal_table <- function(n_rows = 3, cols = c("x", "y")) {
  data.frame(
    x = sample(1000 + seq(n_rows * 100), size = n_rows),
    y = sample(c(0, 1, 2), size = n_rows, replace = TRUE),
    z = 3,
    a = 4,
    b = sample(
      c("Recusandae", "vero", "nihil", "velit", "omnis"),
      size = n_rows,
      replace = TRUE
    ),
    c = sample(
      Sys.time() - 3600 * c(1, 10, 100, 1000),
      size = n_rows,
      replace = TRUE
    ),
    d = sample(Sys.Date() + c(1, -1, 10, -10), size = n_rows, replace = TRUE)
  ) %>%
    select(all_of(cols))
}

#' Minimal metadata data frame mock for a ADaM dataset
#'
#' @param dataset Flag that indicates that the `dataset` column should
#' be included.
#' @param length Flag that indicates that the `length` column should
#' be included.
#' @param label Flag that indicates that the `label` column should
#' be included.
#' @param type Flag that indicates that the `type` column should
#' be included.
#' @param format Flag that indicates that the `format` column should
#' be included.
#' @param order Flag that indicates that the `order` column should
#' be included.
#' @param dataset_name String with name of domain.
#' @param var_names Character vector that defines which variables (rows)
#' to keep
#'
#' @return A metadata data.frame
#' @keywords internal
minimal_metadata <- function(dataset = FALSE,
                             length = FALSE,
                             label = FALSE,
                             type = FALSE,
                             format = FALSE,
                             order = FALSE,
                             dataset_name = "adsl",
                             var_names = NULL) {
  cols_logical <- c(dataset, TRUE, label, length, type, format, order)
  cols <- c(
    "dataset", "variable", "label", "length", "type", "format", "order"
  )[cols_logical]

  metadata <- tribble(
    ~dataset, ~variable, ~label, ~length, ~type, ~format, ~order,
    "adsl", "x", "Lorem", 8, "numeric", NA, 1,
    "adsl", "y", "Ipsum", 200, "numeric", NA, 2,
    "adsl", "z", "Dolor", 8, "numeric", NA, 3,
    "adsl", "a", "Sit", 8, "numeric", NA, 4,
    "adsl", "b", "Amet", 200, "character", NA, 5,
    "adsl", "c", "Consectetur", 200, "character", "datetime20.", 6,
    "adsl", "d", "Adipiscing", 200, "date", "date9.", 7
  )

  if (!is.null(var_names)) {
    metadata <- metadata %>%
      filter(.data$variable %in% var_names)
  }

  metadata %>% select(all_of(cols))
}


#' Local function to remove extra spaces and format by cli
#'
#' Groups together multiple calls instead of being spread out in code
#'
#' @param `[environment(1)]`\cr Attach exit handlers to this environment. Typically, this should
#' be either the current environment or a parent frame
#' (accessed through parent.frame()).
#' @keywords internal
local_cli_theme <- function(.local_envir = parent.frame()) {
  cli_theme_tests <- list(
    h2 = list(`margin-top` = 0, `margin-bottom` = 0, fmt = function(x) x),
    h1 = list(`margin-top` = 0, `margin-bottom` = 0, fmt = function(x) x),
    `.alert` = list(before = NULL),
    `.alert-danger` = list(before = NULL),
    `.alert-success` = list(before = NULL)
  )

  withr::local_options(list(cli.user_theme = cli_theme_tests), .local_envir = .local_envir)
  withr::local_envvar(list(NO_COLOR = "yes"), .local_envir = .local_envir)
  app <- cli::start_app(output = "message", .auto_close = FALSE)
  withr::defer(cli::stop_app(app), envir = .local_envir)
}

#' Test if multiple vars in spec will result in warning message
#' @keywords internal
multiple_vars_in_spec_helper <- function(FUN) {
  adsl <- minimal_table(30)
  metadata <- minimal_metadata(
    dataset = TRUE,
    order = TRUE,
    length = TRUE,
    type = TRUE,
    format = TRUE,
    label = TRUE,
    var_names = colnames(adsl)
  )

  metadata <- metadata %>%
    mutate(dataset = "adtte") %>%
    dplyr::bind_rows(metadata) %>%
    dplyr::rename(Dataset = "dataset")

  withr::local_options(list(xportr.length_verbose = "message"))
  # Setup temporary options with active verbose and Remove empty lines in cli theme
  local_cli_theme()

  adsl %>%
    FUN(metadata) %>%
    testthat::expect_message("There are multiple specs for the same variable name")
}

#' Test if multiple vars in spec with appropriate
#' @keywords internal
multiple_vars_in_spec_helper2 <- function(FUN) {
  adsl <- minimal_table(30)
  metadata <- minimal_metadata(
    dataset = TRUE,
    order = TRUE,
    length = TRUE,
    type = TRUE,
    format = TRUE,
    label = TRUE,
    var_names = colnames(adsl)
  )

  metadata <- metadata %>%
    mutate(dataset = "adtte") %>%
    dplyr::bind_rows(metadata) %>%
    dplyr::rename(Dataset = "dataset")

  withr::local_options(list(xportr.length_verbose = "message", xportr.domain_name = "Dataset"))
  # Setup temporary options with active verbose and Remove empty lines in cli theme
  local_cli_theme()

  adsl %>%
    FUN(metadata) %>%
    testthat::expect_no_message(message = "There are multiple specs for the same variable name")
}

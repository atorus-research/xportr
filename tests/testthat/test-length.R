#' Test `xportr_length()` function
#'
#' Tests will check for:
#'  * Errors
#' * Result of call will create `SASlength` attribute (`width` for each
#' variable)

test_that("xportr_length: Accepts valid domain names in metadata object", {
  adsl <- minimal_table(30)
  metadata <- minimal_metadata(dataset = TRUE, length = TRUE, var_names = colnames(adsl))

  # Setup temporary options with active verbose
  withr::local_options(list(xportr.length_verbose = "message"))

  # Test minimal call with valid data and without domain
  xportr_length(adsl, metadata) %>%
    expect_silent() %>%
    expect_attr_width(metadata$length)

  # Test minimal call with valid data with a valid domain
  xportr_length(adsl, metadata, domain = "adsl") %>%
    expect_silent() %>%
    expect_attr_width(metadata$length) %>%
    NROW() %>%
    expect_equal(30)

  # Test minimal call without datasets
  metadata_without_dataset <- metadata %>% select(-"dataset")

  xportr_length(adsl, metadata_without_dataset) %>%
    expect_silent() %>%
    expect_attr_width(metadata_without_dataset$length) %>%
    NROW() %>%
    expect_equal(30)

  # Test minimal call without datasets and ignores domain
  xportr_length(adsl, metadata_without_dataset, domain = "something_else") %>%
    expect_silent() %>%
    expect_attr_width(metadata_without_dataset$length) %>%
    NROW() %>%
    expect_equal(30)
})

test_that("xportr_length: CDISC data frame is being piped after another xportr function", {
  adsl <- minimal_table(30)
  metadata <- minimal_metadata(
    dataset = TRUE, length = TRUE, type = TRUE, format = TRUE, var_names = colnames(adsl)
  )

  # Setup temporary options with active verbose
  withr::local_options(list(xportr.length_verbose = "message"))

  adsl %>%
    xportr_type(metadata, domain = "adsl", verbose = "message") %>%
    xportr_length(metadata) %>%
    expect_silent() %>%
    expect_attr_width(metadata$length) %>%
    attr("_xportr.df_arg_") %>%
    expect_equal("adsl")
})

test_that("xportr_length: CDISC data frame domain is being recognized from pipe", {
  adsl <- minimal_table(30)
  metadata <- minimal_metadata(dataset = TRUE, length = TRUE, var_names = colnames(adsl))

  # Setup temporary options with `verbose = "message"`
  withr::local_options(list(xportr.length_verbose = "message"))

  # Remove empty lines in cli theme
  local_cli_theme()

  # With domain manually set
  not_adsl <- adsl
  result <- not_adsl %>%
    xportr_length(metadata) %>%
    expect_message("Variable lengths missing from metadata") %>%
    expect_message("lengths resolved") %>%
    expect_message("Variable\\(s\\) present in dataframe but doesn't exist in `metadata`")

  suppressMessages({
    result <- not_adsl %>%
      xportr_length(metadata, verbose = "none")
  })

  expect_no_match(attr(result, "_xportr.df_arg_"), "^adsl$")

  # Test results with piping
  result <- adsl %>%
    xportr_length(metadata)

  attr(result, "_xportr.df_arg_") %>%
    expect_equal("adsl")
})

test_that("xportr_length: Impute character lengths based on class", {
  adsl <- minimal_table(30, cols = c("x", "b"))
  metadata <- minimal_metadata(
    dataset = TRUE, length = TRUE, var_names = colnames(adsl)
  ) %>%
    mutate(length = length - 1)

  # Setup temporary options with `verbose = "none"`
  withr::local_options(list(xportr.length_verbose = "none"))
  # Define controlled `character_types` for this test
  withr::local_options(list(xportr.character_types = c("character", "date")))

  # Remove empty lines in cli theme
  local_cli_theme()

  # Test length imputation of character and numeric (not valid character type)
  result <- adsl %>%
    xportr_length(metadata) %>%
    expect_silent()

  expect_attr_width(result, c(7, 199))

  # Test length imputation of two valid character types (both should have
  # `width = 200``)
  adsl <- adsl %>%
    mutate(
      new_date = as.Date(.data$x, origin = "1970-01-01"),
      new_char = as.character(.data$b),
      new_num = as.numeric(.data$x)
    )

  adsl %>%
    xportr_length(metadata) %>%
    expect_message("Variable lengths missing from metadata") %>%
    expect_message("lengths resolved") %>%
    expect_attr_width(c(7, 199, 200, 200, 8))
})

test_that("xportr_length: Throws message when variables not present in metadata", {
  adsl <- minimal_table(30, cols = c("x", "y"))
  metadata <- minimal_metadata(dataset = TRUE, length = TRUE, var_names = c("x"))

  # Setup temporary options with `verbose = "message"`
  withr::local_options(list(xportr.length_verbose = "message"))
  # Remove empty lines in cli theme
  local_cli_theme()

  # Test that message is given which indicates that variable is not present
  xportr_length(adsl, metadata) %>%
    expect_message("Variable lengths missing from metadata") %>%
    expect_message("lengths resolved") %>%
    expect_message(regexp = "Problem with `y`")
})

test_that("xportr_length: Metacore instance can be used", {
  skip_if_not_installed("metacore")
  adsl <- minimal_table(30, cols = c("x", "b"))

  # Build a minimal metacore object
  metadata <- suppressMessages(
    suppressWarnings(
      metacore::metacore(
        ds_spec = dplyr::tibble(
          dataset = "ADSL"
        ),
        ds_vars = dplyr::tibble(
          dataset = "ADSL",
          variable = colnames(adsl)
        ),
        var_spec = minimal_metadata(
          length = TRUE,
          type = TRUE,
          label = TRUE,
          format = TRUE,
          order = TRUE
        )
      )
    )
  )

  # Test metacore parameter with `metacore` class instead of data.frame
  xportr_length(adsl, metadata, domain = "adsl", verbose = "message") %>%
    expect_silent() %>%
    NROW() %>%
    expect_equal(30) %>%
    expect_attr_width(metadata$length)
})

test_that("xportr_length: Domain not in character format", {
  skip_if_not_installed("haven")
  skip_if_not_installed("readxl")

  require(haven, quietly = TRUE)
  require(readxl, quietly = TRUE)

  ADAE <- read_sas(system.file("extdata", "adae.sas7bdat", package = "xportr"))
  met <- read_excel(system.file("specs", "ADaM_spec.xlsx", package = "xportr"), 3)

  expect_error(
    xportr_length(ADAE, met, domain = ADAE, verbose = "none")
  )
})

test_that("xportr_length: Column length of known/unkown character types is 200/8 ", {
  expect_equal(impute_length(123), 8)
  expect_equal(impute_length(123L), 8)
  expect_equal(impute_length("string"), 200)
  expect_equal(impute_length(Sys.Date()), 200)
  expect_equal(impute_length(Sys.time()), 200)

  withr::local_options(list(xportr.character_types = c("character", "date")))
  expect_equal(impute_length(Sys.time()), 8)
})

test_that("xportr_length: error when metadata is not set", {
  adsl <- minimal_table(30)

  expect_error(
    xportr_length(adsl),
    regexp = "Metadata must be set with `metadata` or `xportr_metadata\\(\\)`"
  )
})

test_that("xportr_length: Gets warning when metadata has multiple rows with same variable", {
  # This test uses the (2) functions below to reduce code duplication
  # All `expect_*` are being called inside the functions
  #
  # Checks that message appears when xportr.domain_name is invalid
  multiple_vars_in_spec_helper(xportr_length)
  # Checks that message doesn't appear when xportr.domain_name is valid
  multiple_vars_in_spec_helper2(xportr_length)
})

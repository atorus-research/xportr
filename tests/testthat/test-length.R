#' Test `xportr_length()` function
#'
#' Tests will check for:
#'  * Errors
#' * Result of call will create SAS default length attribute (`width` for each
#' variable)

# xportr_length
## Test 1: xportr_length: Accepts valid domain names in metadata object ----
test_that("length Test 1: Accepts valid domain names in metadata object", {
  adsl <- minimal_table(30)
  metadata <-
    minimal_metadata(
      dataset = TRUE,
      length = TRUE,
      var_names = colnames(adsl)
    )

  # Setup temporary options with active verbose
  local_options(xportr.length_verbose = "message")

  # Test minimal call with valid data and without domain
  adsl %>%
    xportr_metadata(domain = "adsl") %>%
    xportr_length(metadata) %>%
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

  xportr_length(adsl, metadata_without_dataset, domain = "adsl") %>%
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

## Test 2: xportr_length: CDISC data frame is being piped after another xportr function ----
test_that(
  "length Test 2: CDISC data frame is being piped after another xportr function",
  {
    adsl <- minimal_table(30)
    metadata <- minimal_metadata(
      dataset = TRUE,
      length = TRUE,
      type = TRUE,
      format = TRUE,
      var_names = colnames(adsl)
    )

    # Setup temporary options with active verbose
    local_options(xportr.length_verbose = "message")

    adsl %>%
      xportr_type(metadata, domain = "adsl", verbose = "message") %>%
      xportr_length(metadata) %>%
      expect_silent() %>%
      expect_attr_width(metadata$length) %>%
      attr("_xportr.df_arg_") %>%
      expect_equal("adsl")
  }
)

## Test 3: xportr_length: Impute character lengths based on class ----
test_that("length Test 3: xportr_length: Impute character lengths based on class", {
  adsl <- minimal_table(30, cols = c("x", "b"))
  metadata <- minimal_metadata(
    dataset = TRUE,
    length = TRUE,
    var_names = colnames(adsl)
  ) %>%
    mutate(length = length - 1)

  # Setup temporary options with `verbose = "none"`
  local_options(xportr.length_verbose = "none")
  # Define controlled `character_types` for this test
  local_options(xportr.character_types = c("character", "date"))

  # Remove empty lines in cli theme
  local_cli_theme()

  # Test length imputation of character and numeric (not valid character type)
  result <- adsl %>%
    xportr_length(metadata, domain = "adsl") %>%
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
    xportr_length(metadata, domain = "adsl") %>%
    expect_message("Variable lengths missing from metadata") %>%
    expect_message("lengths resolved") %>%
    expect_attr_width(c(7, 199, 200, 200, 8))
})

## Test 4: xportr_length: Throws message when variables not present in metadata ----
test_that("length Test 4: xportr_length: Throws message when variables not present in metadata", {
  adsl <- minimal_table(30, cols = c("x", "y"))
  metadata <-
    minimal_metadata(
      dataset = TRUE,
      length = TRUE,
      var_names = c("x")
    )

  # Setup temporary options with `verbose = "message"`
  local_options(xportr.length_verbose = "message")
  # Remove empty lines in cli theme
  local_cli_theme()

  # Test that message is given which indicates that variable is not present
  xportr_length(adsl, metadata, domain = "adsl") %>%
    expect_message("Variable lengths missing from metadata") %>%
    expect_message("lengths resolved") %>%
    expect_message(regexp = "Problem with `y`")
})

## Test 5: xportr_length: Metacore instance can be used ----
test_that("length Test 5: Metacore instance can be used", {
  skip_if_not_installed("metacore")
  adsl <- minimal_table(30, cols = c("x", "b"))

  # Build a minimal metacore object
  metadata <- suppressMessages(suppressWarnings(
    metacore::metacore(
      ds_spec = dplyr::tibble(dataset = "ADSL"),
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
  ))

  # Test metacore parameter with `metacore` class instead of data.frame
  xportr_length(adsl, metadata, domain = "adsl", verbose = "message") %>%
    expect_silent() %>%
    NROW() %>%
    expect_equal(30) %>%
    expect_attr_width(metadata$length)
})

## Test 6: xportr_length: Domain not in character format ----
test_that("length Test 6: Domain not in character format", {
  skip_if_not_installed("readxl")

  require(haven, quietly = TRUE)
  require(readxl, quietly = TRUE)

  ADAE <-
    read_sas(system.file("extdata", "adae.sas7bdat", package = "xportr"))
  met <-
    read_excel(system.file("specs", "ADaM_spec.xlsx", package = "xportr"), 3)

  expect_error(xportr_length(ADAE, met, domain = ADAE, verbose = "none"))
})

## Test 7: xportr_length: error when metadata is not set ----
test_that("length Test 7: error when metadata is not set", {
  adsl <- minimal_table(30)

  expect_error(xportr_length(adsl),
    regexp = "Must be of type 'data.frame', 'Metacore' or set via 'xportr_metadata\\(\\)'"
  )
})

## Test 8: xportr_length: Gets warning when metadata has multiple rows with same variable ----
test_that(
  "length Test 8: Gets warning when metadata has multiple rows with same variable",
  {
    # This test uses the (2) functions below to reduce code duplication
    # All `expect_*` are being called inside the functions
    #
    # Checks that message appears when xportr.domain_name is invalid
    multiple_vars_in_spec_helper(xportr_length)
    # Checks that message doesn't appear when xportr.domain_name is valid
    multiple_vars_in_spec_helper2(xportr_length)
  }
)

meta_example <- data.frame(
  dataset = "df",
  variable = c("USUBJID", "WEIGHT"),
  length = c(10, 8)
)

df <- data.frame(
  USUBJID = c("1", "12", "123"),
  WEIGHT = c(85, 45, 121)
)

## Test 9: xportr_length: length assigned as expected from metadata or data ----
test_that("length Test 9: length assigned as expected from metadata or data", {
  result <- df %>%
    xportr_length(meta_example, domain = "df", length_source = "metadata") %>%
    expect_attr_width(c(10, 8))
  suppressMessages(
    result <- df %>%
      xportr_length(meta_example, domain = "df", length_source = "data") %>%
      expect_attr_width(c(3, 8))
  )
})

## Test 10: xportr_length: Gets message when length in metadata longer than data length ----
test_that(
  "length Test 10: Gets message when length in metadata longer than data length",
  {
    result <- df %>%
      xportr_length(meta_example, domain = "df", length_source = "data") %>%
      expect_message()
  }
)

## Test 11: xportr_length: Works as expected with only one domain in metadata ----
test_that("length Test 11: Works as expected with only one domain in metadata", {
  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    BRTHDT = c(1, 1, 2)
  )

  metadata <- data.frame(
    dataset = c("adsl", "adsl"),
    variable = c("USUBJID", "BRTHDT"),
    length = c(1, 1)
  )

  expect_silent(xportr_length(adsl, metadata))
})

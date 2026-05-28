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
  adsl |>
    xportr_metadata(domain = "adsl") |>
    xportr_length(metadata) |>
    expect_silent() |>
    expect_attr_width(metadata$length)

  # Test minimal call with valid data with a valid domain
  xportr_length(adsl, metadata, domain = "adsl") |>
    expect_silent() |>
    expect_attr_width(metadata$length) |>
    NROW() |>
    expect_equal(30)

  # Test minimal call without datasets
  metadata_without_dataset <- metadata |> select(-"dataset")

  xportr_length(adsl, metadata_without_dataset, domain = "adsl") |>
    expect_silent() |>
    expect_attr_width(metadata_without_dataset$length) |>
    NROW() |>
    expect_equal(30)

  # Test minimal call without datasets and ignores domain
  xportr_length(adsl, metadata_without_dataset, domain = "something_else") |>
    expect_silent() |>
    expect_attr_width(metadata_without_dataset$length) |>
    NROW() |>
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

    adsl |>
      xportr_type(metadata, domain = "adsl", verbose = "message") |>
      xportr_length(metadata) |>
      expect_silent() |>
      expect_attr_width(metadata$length) |>
      attr("_xportr.df_arg_") |>
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
  ) |>
    mutate(length = length - 1)

  # Setup temporary options with `verbose = "none"`
  local_options(xportr.length_verbose = "none")
  # Define controlled `character_types` for this test
  local_options(xportr.character_types = c("character", "date"))

  # Remove empty lines in cli theme
  local_cli_theme()

  # Test length imputation of character and numeric (not valid character type)
  result <- adsl |>
    xportr_length(metadata, domain = "adsl") |>
    expect_silent()

  expect_attr_width(result, c(7, 199))

  # Test length imputation of two valid character types (both should have
  # `width = 200``)
  adsl <- adsl |>
    mutate(
      new_date = as.Date(.data$x, origin = "1970-01-01"),
      new_char = as.character(.data$b),
      new_num = as.numeric(.data$x)
    )

  adsl |>
    xportr_length(metadata, domain = "adsl") |>
    expect_message("Variable lengths missing from metadata") |>
    expect_message("lengths resolved") |>
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
  xportr_length(adsl, metadata, domain = "adsl") |>
    expect_message("Variable lengths missing from metadata") |>
    expect_message("lengths resolved") |>
    expect_message(regexp = "Problem with `y`")
})

## Test 5: xportr_length: Metacore instance can be used ----
test_that("length Test 5: Metacore instance can be used", {
  skip_if_not_installed("metacore")
  adsl <- minimal_table(30, cols = c("x", "b"))

  # Build a minimal metacore object
  metadata <- suppressMessages(suppressWarnings(
    metacore::metacore(
      ds_spec = dplyr::tibble(dataset = "ADSL", label = "Subject Level Dataset"),
      ds_vars = dplyr::tibble(
        dataset = "ADSL",
        variable = colnames(adsl)
      ),
      var_spec = minimal_metadata(
        length = TRUE,
        type = TRUE,
        label = TRUE,
        format = TRUE,
        order = TRUE,
        var_names = colnames(adsl)
      )
    )
  ))

  # Test metacore parameter with `metacore` class instead of data.frame
  xportr_length(adsl, metadata, domain = "adsl", verbose = "message") |>
    expect_silent() |>
    NROW() |>
    expect_equal(30) |>
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
  result <- df |>
    xportr_length(meta_example, domain = "df", length_source = "metadata") |>
    expect_attr_width(c(10, 8))
  suppressMessages(
    result <- df |>
      xportr_length(meta_example, domain = "df", length_source = "data") |>
      expect_attr_width(c(3, 8))
  )
})

## Test 10: xportr_length: Gets message when length in metadata longer than data length ----
test_that(
  "length Test 10: Gets message when length in metadata longer than data length",
  {
    # Remove empty lines in cli theme
    local_cli_theme()

    result <- df |>
      xportr_length(meta_example, domain = "df", length_source = "data") |>
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

## Test 12: xportr_length: Throws message when metadata variables not present in dataset ----
test_that("length Test 12: Throws message when metadata variables not present in dataset", {
  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003)
  )

  # Regardless of length values being NA or not, `BRTHDT`, `TRT01A` should be detected
  # by the check because they are both in "adsl" domain. On the other hand,
  # `AETESTCD` should not be detected, as it is in a different domain.
  metadata <- data.frame(
    dataset = c("adsl", "adsl", "adsl", "adae"),
    variable = c("USUBJID", "BRTHDT", "TRT01A", "AETESTCD"),
    length = c(1, NA, 8, 20)
  )

  expect_snapshot({
    xportr_length(adsl, metadata, domain = "adsl", verbose = "message")
  })
})

## Test 13: xportr_length: Works as expected with single row dataset ----
test_that("length Test 13: Works as expected with single row dataset", {
  adsl <- data.frame(
    n1 = c(123),
    c1 = c("abc")
  )

  metadata <- tribble(
    ~dataset, ~variable, ~length, ~type,
    "adsl", "n1", 8, "numeric",
    "adsl", "c1", 200, "character",
  )

  xportr_length(adsl, metadata, domain = "adsl") |>
    expect_silent() |>
    expect_attr_width(metadata$length)
})

## Test 14: xportr_length: Works as expected with mixed NA and non-NA lengths in metadata ----
test_that("length Test 14: Works as expected with mixed NA and non-NA lengths in metadata", {
  metadata <- tribble(
    ~dataset, ~variable, ~length, ~type,
    "adsl", "n1", 8, "numeric",
    "adsl", "c1", 200, "character",
    "adsl", "n2", NA, "numeric",
    "adsl", "c2", NA, "character"
  )

  # Case 1: Variables with only NA values
  adsl_na <- data.frame(
    n1 = c(NA, NA),
    c1 = c(NA_character_, NA_character_),
    n2 = c(NA, NA),
    c2 = c(NA_character_, NA_character_)
  )

  # Case 2: Variables with non-NA values
  adsl_val <- data.frame(
    n1 = c(123, 456),
    c1 = c("abc", "def"),
    n2 = c(789, 910),
    c2 = c("ghi", "jkl")
  )


  suppressMessages({
    xportr_length(adsl_na, metadata, domain = "adsl", length_source = "metadata") |>
      expect_attr_width(c(8, 200, 8, 0))
    xportr_length(adsl_na, metadata, domain = "adsl", length_source = "data") |>
      expect_attr_width(c(8, 0, 8, 0))

    xportr_length(adsl_val, metadata, domain = "adsl", length_source = "metadata") |>
      expect_attr_width(c(8, 200, 8, 3))
    xportr_length(adsl_val, metadata, domain = "adsl", length_source = "data") |>
      expect_attr_width(c(8, 3, 8, 3))
  })
})

## Test 15: xportr_length: Works as expected with zero-length character columns in dataset ----
test_that("length Test 15: Works as expected with zero-length character columns in dataset", {
  adsl <- data.frame(
    b = c("", ""),
    undefined_char_var = c("", "")
  )

  metadata <- minimal_metadata(
    dataset = TRUE,
    length = TRUE,
    var_names = colnames(adsl)
  )

  suppressMessages({
    xportr_length(adsl, metadata, domain = "adsl") |>
      expect_attr_width(c(200, 0))
    xportr_length(adsl, metadata, domain = "adsl", length_source = "data") |>
      expect_attr_width(c(0, 0))
  })
})

## Test 16: xportr_length: Length in metadata or dataset exceeds SAS limits ----
test_that("length Test 16: Length in metadata or dataset exceeds SAS limits", {
  wide_str <- strrep("x", 1000)

  # Case 1: Variable length exceeds SAS limits in metadata
  adsl <- data.frame(C1 = "abc", N1 = 123)
  metadata <- tribble(
    ~dataset, ~variable, ~length, ~type,
    "adsl", "C1", 40000, "character",
    "adsl", "N1", 10000, "numeric"
  )

  xportr_length(adsl, metadata, domain = "adsl") |>
    expect_attr_width(metadata$length) |>
    expect_silent()

  # Case 2: Variable length exceeds SAS limits in dataset (with defined or undefined attributes in metadata)
  adsl <- data.frame(
    C1 = wide_str,
    C2 = wide_str,
    N1 = 99999999999,
    N2 = 99999999999
  )
  metadata <- tribble(
    ~dataset, ~variable, ~length, ~type,
    "adsl", "C1", 200, "character",
    "adsl", "N1", 8, "numeric"
  )

  suppressMessages(
    xportr_length(adsl, metadata, domain = "adsl") |>
      expect_attr_width(c(200, 1000, 8, 8))
  )
})

## Test 17: xportr_length: Gets warning/error as expected for verbose modes 'warn' and 'stop' ----
test_that("length Test 17: Gets warning/error as expected for verbose modes 'warn' and 'stop'", {
  # Case used: Variable(s) present in data but  missing from metadata
  adsl <- minimal_table(30, cols = c("x", "y"))
  metadata <- minimal_metadata(dataset = TRUE, length = TRUE, var_names = "x")

  suppressMessages({
    xportr_length(adsl, metadata, domain = "adsl", verbose = "warn") |>
      expect_warning()

    xportr_length(adsl, metadata, domain = "adsl", verbose = "stop") |>
      expect_error()
  })
})

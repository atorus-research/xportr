# xportr_format ----
## Test 1: xportr_format: error when metadata is not set ----
test_that("format Test 1: error when metadata is not set", {
  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    BRTHDT = c(1, 1, 2)
  )

  expect_error(
    xportr_format(adsl),
    regexp = "Must be of type 'data.frame', 'Metacore' or set via 'xportr_metadata\\(\\)'"
  )
})

## Test 2: xportr_format: Gets warning when metadata has multiple rows with same variable ----
test_that("format Test 2: Gets warning when metadata has multiple rows with same variable", {
  # This test uses the (2) functions below to reduce code duplication
  # All `expect_*` are being called inside the functions
  #
  # Checks that message appears when xportr.domain_name is invalid
  multiple_vars_in_spec_helper(xportr_format)
  # Checks that message doesn't appear when xportr.domain_name is valid
  multiple_vars_in_spec_helper2(xportr_format)
})

## Test 3: xportr_format: Works as expected with only one domain in metadata ----
test_that("format Test 3: Works as expected with only one domain in metadata", {
  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    BRTHDT = c(1, 1, 2)
  )

  metadata <- data.frame(
    dataset = c("adsl", "adsl"),
    variable = c("USUBJID", "BRTHDT"),
    format = c(NA, "DATE9.")
  )

  expect_silent(xportr_format(adsl, metadata))
})

## Test 4: xportr_format: Variable ending in DT should produce a warning if no format ----
test_that("format Test 4: Variable ending in DT should produce a warning if no format", {
  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    BRTHDT = c(1, 1, 2)
  )

  metadata <- data.frame(
    dataset = c("adsl", "adsl"),
    variable = c("USUBJID", "BRTHDT"),
    format = c(NA, NA)
  )

  expect_warning(
    xportr_format(adsl, metadata, verbose = "warn"),
    regexp = "(xportr::xportr_format) `BRTHDT` is expected to have a format but does not.",
    fixed = TRUE
  )
})

## Test 5: xportr_format: Variable ending in TM should produce an error if no format ----
test_that("format Test 5: Variable ending in TM should produce an error if no format", {
  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    BRTHTM = c(1, 1, 2)
  )

  metadata <- data.frame(
    dataset = c("adsl", "adsl"),
    variable = c("USUBJID", "BRTHTM"),
    format = c(NA, NA)
  )

  expect_error(
    xportr_format(adsl, metadata, verbose = "stop"),
    regexp = "(xportr::xportr_format) `BRTHTM` is expected to have a format but does not.",
    fixed = TRUE
  )
})

## Test 6: xportr_format: Variable ending in DTM should produce a warning if no format ----
test_that("format Test 6: Variable ending in DTM should produce a warning if no format", {
  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    BRTHDTM = c(1, 1, 2)
  )

  metadata <- data.frame(
    dataset = c("adsl", "adsl"),
    variable = c("USUBJID", "BRTHDTM"),
    format = c(NA, NA)
  )

  expect_warning(
    xportr_format(adsl, metadata, verbose = "warn"),
    regexp = "(xportr::xportr_format) `BRTHDTM` is expected to have a format but does not.",
    fixed = TRUE
  )
})

test_that(
  "xportr_format: If a variable is character then an error should be produced if format does not start with `$`",
  {
    adsl <- data.frame(
      USUBJID = c("1001", "1002", "1003"),
      BRTHDT = c(1, 1, 2)
    )

    metadata <- data.frame(
      dataset = c("adsl", "adsl"),
      variable = c("USUBJID", "BRTHDT"),
      format = c("4.", "DATE9.")
    )

    expect_error(
      xportr_format(adsl, metadata, verbose = "stop"),
      regexp = "(xportr::xportr_format) `USUBJID` is a character variable and should have a `$` prefix.",
      fixed = TRUE
    )
  }
)

## Test 7: xportr_format: If a variable is character then a warning should be produced if format is > 32 in length ----
test_that("format Test 7: If a variable is character then a warning should be produced if format is > 32 in length", { # nolint
  adsl <- data.frame(
    USUBJID = c("1001", "1002", "1003"),
    BRTHDT = c(1, 1, 2)
  )

  metadata <- data.frame(
    dataset = c("adsl", "adsl"),
    variable = c("USUBJID", "BRTHDT"),
    format = c("$AVERYLONGFORMATNAMEWHICHISGREATERTHAN32.", "DATE9.")
  )

  res <- evaluate_promise(xportr_format(adsl, metadata, verbose = "warn"))

  expect_equal(
    res$warnings,
    c(
      "(xportr::xportr_format) Format for character variable `USUBJID` should have length <= 31 (excluding `$`).",
      paste0(
        "(xportr::xportr_format) Check format ",
        "`$AVERYLONGFORMATNAMEWHICHISGREATERTHAN32.` for variable ",
        "`USUBJID` - is this correct?"
      )
    )
  )
})

## Test 8: xportr_format: If a variable is numeric then an error should be produced if a format starts with `$` ----
test_that(
  "format Test 8: If a variable is numeric then an error should be produced if a format starts with `$`",
  { # nolint
    adsl <- data.frame( # nolint
      USUBJID = c(1001, 1002, 1003),
      BRTHDT = c(1, 1, 2)
    )

    metadata <- data.frame( # nolint
      dataset = c("adsl", "adsl"),
      variable = c("USUBJID", "BRTHDT"),
      format = c("$4.", "DATE9.")
    )

    expect_error( # nolint
      xportr_format(adsl, metadata, verbose = "stop"),
      regexp = "(xportr::xportr_format) `USUBJID` is a numeric variable and should not have a `$` prefix.",
      fixed = TRUE
    )
  }
)

## Test 9: xportr_format: If a variable is numeric then a warning should be produced if format is > 32 in length ----      #nolint
test_that("format Test 9: If a variable is numeric then a warning should be produced if format is > 32 in length", { # nolint
  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    BRTHDT = c(1, 1, 2)
  )

  metadata <- data.frame(
    dataset = c("adsl", "adsl"),
    variable = c("USUBJID", "BRTHDT"),
    format = c("AVERYLONGFORMATNAMEWHICHISGREATERTHAN32.", "DATE9.")
  )

  res <- evaluate_promise(xportr_format(adsl, metadata, verbose = "warn"))

  expect_equal(
    res$warnings,
    c(
      "(xportr::xportr_format) Format for numeric variable `USUBJID` should have length <= 32.",
      paste0(
        "(xportr::xportr_format) Check format ",
        "`AVERYLONGFORMATNAMEWHICHISGREATERTHAN32.` for variable ",
        "`USUBJID` - is this correct?"
      )
    )
  )
})

test_that(
  "xportr_format: If a format is not one of the expected formats identified, then a message should be produced",
  {
    adsl <- data.frame(
      USUBJID = c(1001, 1002, 1003),
      BRTHDT = c(1, 1, 2)
    )

    metadata <- data.frame(
      dataset = c("adsl", "adsl"),
      variable = c("USUBJID", "BRTHDT"),
      format = c("NOTASTDFMT.", "DATE9.")
    )

    expect_message(
      xportr_format(adsl, metadata, verbose = "message"),
      regexp = "(xportr::xportr_format) Check format `NOTASTDFMT.` for variable `USUBJID` - is this correct?",
      fixed = TRUE
    )
  }
)

test_that(
  "xportr_format: Check for case-sensitivity. Using lowercase should not affect anything",
  {
    adsl <- data.frame(
      USUBJID = c(1001, 1002, 1003),
      BRTHDT = c(1, 1, 2),
      BRTHTM = c(2, 2, 5)
    )

    metadata <- data.frame(
      dataset = c("adsl", "adsl", "adsl"),
      variable = c("USUBJID", "BRTHDT", "BRTHTM"),
      format = c(NA, "date9.", "time5.")
    )

    expect_silent(xportr_format(adsl, metadata))
  }
)

test_that(
  "xportr_format: Check for case-sensitivity. Using mixed case should not affect anything",
  {
    adsl <- data.frame(
      USUBJID = c(1001, 1002, 1003),
      BRTHDT = c(1, 1, 2),
      BRTHTM = c(2, 2, 5)
    )

    metadata <- data.frame(
      dataset = c("adsl", "adsl", "adsl"),
      variable = c("USUBJID", "BRTHDT", "BRTHTM"),
      format = c(NA, "daTe9.", "TimE5.")
    )

    expect_silent(xportr_format(adsl, metadata))
  }
)

test_that(
  "xportr_format: Check for case-sensitivity. Using a mixture of case should not affect anything",
  {
    adsl <- data.frame(
      USUBJID = c(1001, 1002, 1003),
      BRTHDT = c(1, 1, 2),
      BRTHTM = c(2, 2, 5),
      BRTHDTM = c(3, 5, 7)
    )

    metadata <- data.frame(
      dataset = c("adsl", "adsl", "adsl", "adsl"),
      variable = c("USUBJID", "BRTHDT", "BRTHTM", "BRTHDTM"),
      format = c(NA, "DATE9.", "time5.", "DaTeTiMe16.")
    )

    expect_silent(xportr_format(adsl, metadata))
  }
)

test_that(
  "xportr_format: If 'verbose' option is 'none', then no messaging should appear",
  {
    adsl <- data.frame(
      USUBJID = c(1001, 1002, 1003),
      BRTHDT = c(1, 1, 2)
    )

    metadata <- data.frame(
      dataset = c("adsl", "adsl"),
      variable = c("USUBJID", "BRTHDT"),
      format = c("NOTASTDFMT.", NA)
    )

    expect_silent(
      xportr_format(adsl, metadata, verbose = "none")
    )
  }
)

test_that("xportr_format() applies formats correctly on ungrouped data", {
  skip_if_not_installed("dplyr")

  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    BRTHDT = c(1, 1, 2),
    stringsAsFactors = FALSE
  )

  metadata <- data.frame(
    dataset = c("adsl", "adsl"),
    variable = c("USUBJID", "BRTHDT"),
    format = c(NA, "DATE9."),
    stringsAsFactors = FALSE
  )

  expect_silent(
    out <- xportr_format(adsl, metadata = metadata, domain = "adsl", verbose = "none")
  )

  expect_identical(nrow(out), nrow(adsl))
  expect_false(dplyr::is_grouped_df(out))

  # USUBJID has no format; BRTHDT should have DATE9.
  expect_identical(attr(out$BRTHDT, "format.sas"), "DATE9.")
})

test_that("xportr_format() warns and preserves grouping when verbose = 'warn'", {
  skip_if_not_installed("dplyr")

  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    BRTHDT = c(1, 1, 2),
    stringsAsFactors = FALSE
  )

  metadata <- data.frame(
    dataset = c("adsl", "adsl"),
    variable = c("USUBJID", "BRTHDT"),
    format = c(NA, "DATE9."),
    stringsAsFactors = FALSE
  )

  grouped <- dplyr::group_by(adsl, USUBJID)

  expect_true(dplyr::is_grouped_df(grouped))

  # group_data_check should emit a warning about grouping
  expect_warning(
    out <- xportr_format(grouped, metadata = metadata, domain = "adsl", verbose = "warn"),
    "Input data is grouped by:",
    fixed = FALSE
  )

  # Grouping is preserved; formats applied
  expect_true(dplyr::is_grouped_df(out))
  expect_identical(attr(out$BRTHDT, "format.sas"), "DATE9.")
})

test_that("xportr_format() messages and preserves grouping when verbose = 'message'", {
  skip_if_not_installed("dplyr")

  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    BRTHDT = c(1, 1, 2),
    stringsAsFactors = FALSE
  )

  metadata <- data.frame(
    dataset = c("adsl", "adsl"),
    variable = c("USUBJID", "BRTHDT"),
    format = c(NA, "DATE9."),
    stringsAsFactors = FALSE
  )

  grouped <- dplyr::group_by(adsl, USUBJID)

  expect_message(
    out <- xportr_format(grouped, metadata = metadata, domain = "adsl", verbose = "message"),
    "Input data is grouped by:",
    fixed = FALSE
  )

  expect_true(dplyr::is_grouped_df(out))
  expect_identical(attr(out$BRTHDT, "format.sas"), "DATE9.")
})

test_that("xportr_format() treats NULL and 'none' as 'warn' for grouped data", {
  skip_if_not_installed("dplyr")

  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    BRTHDT = c(1, 1, 2),
    stringsAsFactors = FALSE
  )

  metadata <- data.frame(
    dataset = c("adsl", "adsl"),
    variable = c("USUBJID", "BRTHDT"),
    format = c(NA, "DATE9."),
    stringsAsFactors = FALSE
  )

  grouped1 <- dplyr::group_by(adsl, USUBJID)
  grouped2 <- dplyr::group_by(adsl, USUBJID)

  # NULL → treated as "warn" in group_data_check
  expect_warning(
    out_null <- xportr_format(grouped1, metadata = metadata, domain = "adsl", verbose = NULL),
    "Input data is grouped by:",
    fixed = FALSE
  )
  expect_true(dplyr::is_grouped_df(out_null))
  expect_identical(attr(out_null$BRTHDT, "format.sas"), "DATE9.")

  # "none" → treated as "warn" in group_data_check
  expect_warning(
    out_none <- xportr_format(grouped2, metadata = metadata, domain = "adsl", verbose = "none"),
    "Input data is grouped by:",
    fixed = FALSE
  )
  expect_true(dplyr::is_grouped_df(out_none))
  expect_identical(attr(out_none$BRTHDT, "format.sas"), "DATE9.")
## Test 10: xportr_format: Uses xportr.format_verbose for a format warning ----
test_that("format Test 10: Uses xportr.format_verbose for a format warning", {
  # Test data with format violation - character variable missing $ prefix
  adsl <- data.frame(
    USUBJID = c("1001", "1002", "1003")
  )

  metadata <- data.frame(
    dataset = "adsl",
    variable = "USUBJID",
    format = "4."
  )

  # Test that xportr.format_verbose controls the format warning
  local_options(xportr.format_verbose = "warn")

  expect_warning(
    xportr_format(adsl, metadata, domain = "adsl"),
    regexp = "(xportr::xportr_format) `USUBJID` is a character variable and should have a `$` prefix.",
    fixed = TRUE
  )

  # Test that xportr.length_verbose no longer affects the format warning
  local_options(xportr.format_verbose = "none", xportr.length_verbose = "warn")

  expect_silent(
    xportr_format(adsl, metadata, domain = "adsl")
  )
})

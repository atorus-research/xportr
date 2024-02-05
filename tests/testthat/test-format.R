test_that("xportr_format: error when metadata is not set", {
  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    BRTHDT = c(1, 1, 2)
  )

  expect_error(
    xportr_format(adsl),
    regexp = "Must be of type 'data.frame', 'Metacore' or set via 'xportr_metadata\\(\\)'"
  )
})

test_that("xportr_format: Gets warning when metadata has multiple rows with same variable", {
  # This test uses the (2) functions below to reduce code duplication
  # All `expect_*` are being called inside the functions
  #
  # Checks that message appears when xportr.domain_name is invalid
  multiple_vars_in_spec_helper(xportr_format)
  # Checks that message doesn't appear when xportr.domain_name is valid
  multiple_vars_in_spec_helper2(xportr_format)
})

test_that("xportr_format: Works as expected with only one domain in metadata", {
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

test_that("xportr_format: Variable ending in DT should produce a warning if no format", {
  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    BRTHDT = c(1, 1, 2)
  )

  metadata <- data.frame(
    dataset = c("adsl", "adsl"),
    variable = c("USUBJID", "BRTHDT"),
    format = c(NA, NA)
  )

  expect_warning(xportr_format(adsl, metadata), regexp = "(xportr::xportr_format) `BRTHDT` is expected to have a format but does not.", fixed = TRUE)
})

test_that("xportr_format: Variable ending in TM should produce a warning if no format", {
  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    BRTHTM = c(1, 1, 2)
  )

  metadata <- data.frame(
    dataset = c("adsl", "adsl"),
    variable = c("USUBJID", "BRTHTM"),
    format = c(NA, NA)
  )

  expect_warning(xportr_format(adsl, metadata), regexp = "(xportr::xportr_format) `BRTHTM` is expected to have a format but does not.", fixed = TRUE)
})

test_that("xportr_format: Variable ending in DTM should produce a warning if no format", {
  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    BRTHDTM = c(1, 1, 2)
  )

  metadata <- data.frame(
    dataset = c("adsl", "adsl"),
    variable = c("USUBJID", "BRTHDTM"),
    format = c(NA, NA)
  )

  expect_warning(xportr_format(adsl, metadata), regexp = "(xportr::xportr_format) `BRTHDTM` is expected to have a format but does not.", fixed = TRUE)
})

test_that("xportr_format: If a variable is character then a warning should be produced if format does not start with `$`", {
  adsl <- data.frame(
    USUBJID = c("1001", "1002", "1003"),
    BRTHDT = c(1, 1, 2)
  )

  metadata <- data.frame(
    dataset = c("adsl", "adsl"),
    variable = c("USUBJID", "BRTHDT"),
    format = c("4.", "DATE9.")
  )

  expect_warning(xportr_format(adsl, metadata), regexp = "(xportr::xportr_format) `USUBJID` is a character variable and should have a `$` prefix.", fixed = TRUE)
})

test_that("xportr_format: If a variable is character then a warning should be produced if format is > 32 in length", {
  adsl <- data.frame(
    USUBJID = c("1001", "1002", "1003"),
    BRTHDT = c(1, 1, 2)
  )

  metadata <- data.frame(
    dataset = c("adsl", "adsl"),
    variable = c("USUBJID", "BRTHDT"),
    format = c("$AVERYLONGFORMATNAMEWHICHISGREATERTHAN32.", "DATE9.")
  )

  expect_warning(xportr_format(adsl, metadata), regexp = "(xportr::xportr_format) Format for character variable `USUBJID` should have length <= 31 (excluding `$`)", fixed = TRUE)
})

## Test 1: xportr_label: error when metadata is not set ----
test_that("label Test 1: xportr_label: error when metadata is not set", {
  df <- data.frame(
    Subj = as.character(123, 456, 789),
    Different = c("a", "b", "c"),
    Val = c("1", "2", "3"),
    Param = c("param1", "param2", "param3")
  )

  expect_error(xportr_label(df),
    regexp = "Must be of type 'data.frame', 'Metacore' or set via 'xportr_metadata\\(\\)'"
  )
})

## Test 2: xportr_label: Gets warning when metadata has multiple rows with same variable ----
test_that(
  "label Test 2: xportr_label: Gets warning when metadata has multiple rows with same variable",
  {
    # This test uses the (2) functions below to reduce code duplication
    # All `expect_*` are being called inside the functions
    #
    # Checks that message appears when xportr.domain_name is invalid
    multiple_vars_in_spec_helper(xportr_label)
    # Checks that message doesn't appear when xportr.domain_name is valid
    multiple_vars_in_spec_helper2(xportr_label)
  }
)


## Test 3: xportr_label: Works as expected with only one domain in metadata ----
test_that("label Test 3: xportr_label: Works as expected with only one domain in metadata", {
  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    BRTHDT = c(1, 1, 2)
  )

  metadata <- data.frame(
    dataset = c("adsl", "adsl"),
    variable = c("USUBJID", "BRTHDT"),
    label = c("Hello", "Hello2")
  )

  expect_silent(xportr_label(adsl, metadata))
})

## Test 4: xportr_label: Reports metadata variables not in dataset ----
test_that("label Test 4: xportr_label: Reports metadata variables not in dataset", {
  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003)
  )

  # Regardless of label values being NA or not, `BRTHDT`, `TRT01A` should be detected
  # by the check because they are both in "adsl" domain. On the other hand,
  # `AETESTCD` should not be detected, as it is in a different domain.
  metadata <- data.frame(
    dataset = c("adsl", "adsl", "adsl", "adae"),
    variable = c("USUBJID", "BRTHDT", "TRT01A", "AETESTCD"),
    label = c("Hello", NA, "Hello3", "Hello4")
  )

  expect_snapshot({
    xportr_label(adsl, metadata, domain = "adsl", verbose = "warn")
  })
})

## Test 5: xportr_label: gives proper verbose output for long label
test_that("label Test 5: xportr_label: gives proper verbose output for long label", {
  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    SITEID = c(001, 002, 003),
    AGE = c(63, 35, 27),
    SEX = c("M", "F", "M")
  )

  metadata <- data.frame(
    dataset = "adsl",
    variable = c("USUBJID", "SITEID", "AGE", "SEX"),
    label = c("Unique Subject Identifier Identifier Identifier", "Study Site Identifier", "Age", "Sex")
  )


  expect_warning(
    xportr_label(adsl, metadata, domain = "adsl", verbose = "warn"),
    "Length of variable label must be 40 characters or less"
  )

  expect_message(
    xportr_label(adsl, metadata, domain = "adsl", verbose = "message"),
    "Length of variable label must be 40 characters or less"
  )
  expect_error(
    xportr_label(adsl, metadata, domain = "adsl", verbose = "stop"),
    "Length of variable label must be 40 characters or less"
  )
})

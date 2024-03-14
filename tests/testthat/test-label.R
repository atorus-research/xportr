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

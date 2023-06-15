test_that("xportr_label: error when metadata is not set", {
  df <- data.frame(
    Subj = as.character(123, 456, 789),
    Different = c("a", "b", "c"),
    Val = c("1", "2", "3"),
    Param = c("param1", "param2", "param3")
  )

  expect_error(
    xportr_label(df),
    regexp = "Metadata must be set with `metadata` or `xportr_metadata\\(\\)`"
  )
})

test_that("xportr_label: Gets warning when metadata has multiple rows with same variable", {
  # This test uses the (2) functions below to reduce code duplication
  # All `expect_*` are being called inside the functions
  #
  # Checks that message appears when xportr.domain_name is invalid
  multiple_vars_in_spec_helper(xportr_label)
  # Checks that message doesn't appear when xportr.domain_name is valid
  multiple_vars_in_spec_helper2(xportr_label)
})

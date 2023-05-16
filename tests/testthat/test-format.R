test_that("xportr_format: error when metadata is not set", {
  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    BRTHDT = c(1, 1, 2)
  )

  expect_error(
    xportr_format(adsl),
    regexp = "Metadata must be set with `metacore` or `xportr_metadata\\(\\)`"
  )
})

test_that("xportr_df_label: error when metadata is not set", {
  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    SITEID = c(001, 002, 003),
    AGE = c(63, 35, 27),
    SEX = c("M", "F", "M")
  )


  expect_error(
    xportr_df_label(adsl),
    regexp = "Metadata must be set with `metadata` or `xportr_metadata\\(\\)`"
  )
})

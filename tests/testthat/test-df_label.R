test_that("xportr_df_label: error when metadata is not set", {
  adsl <- minimal_table()

  expect_error(
    xportr_df_label(adsl),
    regexp = "Metadata must be set with `metadata` or `xportr_metadata\\(\\)`"
  )
})

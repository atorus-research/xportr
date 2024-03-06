## Test 1: xportr_df_label: error when metadata is not set ----
test_that("df_label Test 1: xportr_df_label: error when metadata is not set", {
  adsl <- minimal_table()

  expect_error(
    xportr_df_label(adsl),
    regexp = "Must be of type 'data.frame', 'Metacore' or set via 'xportr_metadata\\(\\)'"
  )
})

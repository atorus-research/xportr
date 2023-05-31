test_that("xportr_label: error when metadata is not set", {
  df <- data.frame(
    Subj = as.character(123, 456, 789),
    Different = c("a", "b", "c"),
    Val = c("1", "2", "3"),
    Param = c("param1", "param2", "param3")
  )

  expect_error(
    xportr_label(df),
    regexp = "Metadata must be set with `metacore` or `xportr_metadata\\(\\)`"
  )
})

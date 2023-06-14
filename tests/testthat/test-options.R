test_that("options are originally set as expected", {
  op <- options()

  expect_equal(op$xportr.df_domain_name, "dataset")
  expect_equal(op$xportr.df_label, "label")
  expect_equal(op$xportr.domain_name, "dataset")
  expect_equal(op$xportr.variable_name, "variable")
  expect_equal(op$xportr.type_name, "type")
  expect_equal(op$xportr.label, "label")
  expect_equal(op$xportr.length, "length")
  expect_equal(op$xportr.format_name, "format")
})

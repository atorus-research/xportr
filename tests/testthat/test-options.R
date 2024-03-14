# xportr_options ----
## Test 1: options are originally set as expected ----
test_that("options Test 1: options are originally set as expected", {
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

## Test 2: xportr_options: options can be fetched using the xportr_options ----
test_that("options Test 2: xportr_options: options can be fetched using the xportr_options", {
  expect_equal(xportr_options(), xportr_options_list)
  new_domain <- "new domain name"
  new_label <- "new label name"
  op <- options(xportr.df_domain_name = new_domain, xportr.df_label = new_label)
  on.exit(options(op), add = TRUE, after = FALSE)
  domain <- xportr_options("xportr.df_domain_name")$xportr.df_domain_name
  domain_label <- xportr_options(c("xportr.df_domain_name", "xportr.df_label"))

  expect_equal(domain, new_domain)
  expect_equal(domain_label, list(xportr.df_domain_name = new_domain, xportr.df_label = new_label))
})

## Test 3: xportr_options: options can be set using the xportr_options ----
test_that("options Test 3: options can be set using the xportr_options", {
  op <- options()
  on.exit(options(op), add = TRUE, after = FALSE)
  old_name <- "old name"
  new_name <- "new name"
  old_label <- "old label"
  new_label <- "new label"
  options(xportr.df_domain_name = old_name, xportr.df_label = old_label)
  old_values <- xportr_options(c("xportr.df_domain_name", "xportr.df_label"))
  expect_equal(old_values, list(xportr.df_domain_name = old_name, xportr.df_label = old_label))

  xportr_options(xportr.df_domain_name = new_name, xportr.df_label = new_label)
  new_values <- xportr_options(c("xportr.df_domain_name", "xportr.df_label"))
  expect_equal(new_values, list(xportr.df_domain_name = new_name, xportr.df_label = new_label))
})

test_that("SAS Transport file", {

  tmpdir <- tempdir()
  tmp <- file.path(tmpdir, "xyz.xpt")
  
  on.exit(unlink(tmpdir))
  
  df <- data.frame(x = c(1, 2, NA), y = c("a", "", "c"), z = c(1, 2, 3))

  SASxport::SASformat(df$x) <- "date7."
  SASxport::label(df$x) <- "foo"
  SASxport::label(df$y) <- "bar"
  SASxport::label(df$z) <- "baz"

  xportr_write(df, path = tmp)
  expect_output(str(SASxport::read.xport(tmp)), "$ X: labelled, format", fixed =TRUE)
  expect_output(str(SASxport::read.xport(tmp)), "$ Y: 'labelled' chr", fixed =TRUE)
  expect_output(str(SASxport::read.xport(tmp)), "$ Z: 'labelled' int", fixed =TRUE)

  xportr_write(df, path = tmp, label = "Lorem ipsum dolor sit amet")
  expect_output(str(SASxport::read.xport(tmp), "Lorem ipsum dolor sit amet"))

  xportr_write(df, path = tmp, label = "Lorem ipsum dolor sit amet")
  expect_error(
    xportr_write(df, tmp, label = "Lorizzle ipsizzle dolizzle pizzle go to hizzle"),
    "must be 40 characters or less")
  expect_error(
    xportr_write(df, tmp, label = "Lorizzle ipsizzle dolizzl\xe7 pizzle"),
    "cannot contain any non-ASCII")

  df <- data.frame(loremipsum = "a", y_ = 1)
  SASxport::label(df$y_) <- "var2"
  expect_error(
    xportr_write(df, tmp),
    "The following validation failed")
})

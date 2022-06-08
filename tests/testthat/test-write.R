test_that("SAS Transport file", {

  tmpdir <- tempdir()
  tmp <- file.path(tmpdir, "xyz.xpt")
  
  on.exit(unlink(tmpdir))
  
  df <- data.frame(X = c(1, 2, NA), Y = c("a", "", "c"), Z = c(1, 2, 3))

  #SASxport::SASformat(df$x, "format") <- "date7."
  attr(df$X, "label") <- "foo"
  attr(df$Y, "label") <- "bar"
  attr(df$Z, "label") <- "baz"

  xportr_write(df, path = tmp)
  #expect_output(str(read_xpt(tmp)), "$ X: labelled, format", fixed =TRUE)
  # expect_output(str(read_xpt(tmp)), "$ Y: 'labelled' chr", fixed = TRUE)
  # expect_output(str(SASxport::read.xport(tmp)), "$ Z: 'labelled' int", fixed = TRUE)

  xportr_write(df, path = tmp, label = "Lorem ipsum dolor sit amet")
  expect_output(str(read_xpt(tmp)), "Lorem ipsum dolor sit amet")

  xportr_write(df, path = tmp, label = "Lorem ipsum dolor sit amet")
  expect_error(
    xportr_write(df, tmp, label = "Lorizzle ipsizzle dolizzle pizzle go to hizzle"),
    "must be 40 characters or less")
  expect_error(xportr_write(df, tmp, label = "Lorizzle ipsizzle dolizzl\xe7 pizzle"))

  # df <- data.frame(loremipsum = "a", y_ = 1)
  # attr(df$y_, "label") <- "var2"
  # expect_warning(
  #   xportr_write(df, tmp),
  #   "Truncated 1 long names to 8 characters.")
})

test_that("Error message given if file name is greater than 8 characters",{

  tmpdir <- tempdir()
  tmp <- file.path(tmpdir, "abc.xpt")

  on.exit(unlink(tmpdir))

  nameover8 <- data.frame(a  = c(1, 2, NA),
                          b = c("a", "", "c"),
                          c = c(1, 2, 3))

  expect_error(xportr_write(df, path = tmp))
  

})

data_to_save <- dplyr::tibble(X = c(1, 2, NA), Y = c("a", "", "c"), Z = c(1, 2, 3))

test_that("xportr_write: exported data can be saved to a file", {
  tmpdir <- tempdir()
  tmp <- file.path(tmpdir, "xyz.xpt")

  on.exit(unlink(tmpdir))

  xportr_write(data_to_save, path = tmp)
  expect_equal(read_xpt(tmp), data_to_save)
})

test_that("xportr_write: exported data can be saved to a file with a label", {
  tmpdir <- tempdir()
  tmp <- file.path(tmpdir, "xyz.xpt")

  on.exit(unlink(tmpdir))

  xportr_write(data_to_save, path = tmp, label = "Lorem ipsum dolor sit amet")
  expect_output(str(read_xpt(tmp)), "Lorem ipsum dolor sit amet")
})

test_that("xportr_write: expect error when invalid multibyte string is passed in label", {
  tmpdir <- tempdir()
  tmp <- file.path(tmpdir, "xyz.xpt")

  on.exit(unlink(tmpdir))

  expect_error(xportr_write(data_to_save, tmp, label = "Lorizzle ipsizzle dolizzl\xe7 pizzle"))
})

test_that("xportr_write: expect error when file name is over 8 characters long", {
  tmpdir <- tempdir()
  tmp <- file.path(tmpdir, paste0(paste(letters[1:9], collapse = ""), ".xpt"))

  on.exit(unlink(tmpdir))

  expect_error(xportr_write(data_to_save, tmp, label = "asdf"))
})

test_that("xportr_write: expect error when file name contains non-ASCII symbols or special characters", {
  tmpdir <- tempdir()
  tmp <- file.path(tmpdir, "<test>.xpt")

  on.exit(unlink(tmpdir))

  expect_error(xportr_write(data_to_save, tmp, label = "asdf"))
})

test_that("xportr_write: expect error when label contains non-ASCII symbols or special characters", {
  tmpdir <- tempdir()
  tmp <- file.path(tmpdir, "xyz.xpt")

  on.exit(unlink(tmpdir))

  expect_error(xportr_write(data_to_save, tmp, label = "çtestç"))
})

test_that("xportr_write: expect error when label is over 40 characters", {
  tmpdir <- tempdir()
  tmp <- file.path(tmpdir, "xyz.xpt")

  on.exit(unlink(tmpdir))

  expect_error(xportr_write(data_to_save, tmp, label = paste(rep("a", 41), collapse = "")))
})

test_that("xportr_write: expect error when an xpt validation fails with strict_checks set to TRUE", {
  tmpdir <- tempdir()
  tmp <- file.path(tmpdir, "xyz.xpt")
  attr(data_to_save$X, "format.sas") <- "foo"

  on.exit(unlink(tmpdir))

  expect_error(xportr_write(data_to_save, tmp, label = "label", strict_checks = TRUE))
})

test_that("xportr_write: expect warning when an xpt validation fails with strict_checks set to FALSE", {
  tmpdir <- tempdir()
  tmp <- file.path(tmpdir, "xyz.xpt")
  attr(data_to_save$X, "format.sas") <- "foo"

  on.exit(unlink(tmpdir))

  expect_warning(xportr_write(data_to_save, tmp, label = "label", strict_checks = FALSE))
})

test_that("xportr_write: expect warning when an xpt validation fails with strict_checks set to FALSE", {
  tmpdir <- tempdir()
  tmp <- file.path(tmpdir, "xyz.xpt")
  attr(data_to_save$X, "format.sas") <- "foo"

  on.exit(unlink(tmpdir))

  expect_warning(xportr_write(data_to_save, tmp, label = "label", strict_checks = FALSE))
})

test_that("xportr_write: Capture errors by haven and report them as such", {
  tmpdir <- tempdir()
  tmp <- file.path(tmpdir, "xyz.xpt")
  attr(data_to_save$X, "format.sas") <- "E8601LXw.asdf"

  on.exit(unlink(tmpdir))

  expect_error(
    suppressWarnings(xportr_write(data_to_save, tmp, label = "label", strict_checks = FALSE)),
    "Error reported by haven"
  )
})

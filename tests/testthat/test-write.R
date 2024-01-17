data_to_save <- dplyr::tibble(X = c(1, 2, NA), Y = c("a", "", "c"), Z = c(1, 2, 3))

test_that("xportr_write: exported data can be saved to a file", {
  tmp <- withr::local_file("xyz.xpt")

  xportr_write(data_to_save, path = tmp)
  expect_equal(read_xpt(tmp), data_to_save)
})

test_that("xportr_write: exported data can still be saved to a file with a label", {
  tmp <- withr::local_file("xyz.xpt")

  suppressWarnings(
    xportr_write(data_to_save,
      path = tmp,
      label = "Lorem ipsum dolor sit amet",
      domain = "data_to_save"
    )
  )
  expect_output(str(read_xpt(tmp)), "Lorem ipsum dolor sit amet")
})

test_that("xportr_write: exported data can be saved to a file with a metadata", {
  tmp <- withr::local_file("xyz.xpt")

  xportr_write(
    data_to_save,
    path = tmp,
    domain = "data_to_save",
    metadata = data.frame(
      dataset = "data_to_save",
      label = "Lorem ipsum dolor sit amet"
    )
  )
  expect_output(str(read_xpt(tmp)), "Lorem ipsum dolor sit amet")
})

test_that("xportr_write: exported data can be saved to a file with a existing metadata", {
  tmp <- withr::local_file("xyz.xpt")

  df <- xportr_df_label(
    data_to_save,
    domain = "data_to_save",
    data.frame(
      dataset = "data_to_save",
      label = "Lorem ipsum dolor sit amet"
    )
  )

  xportr_write(df, path = tmp, domain = "data_to_save")
  expect_output(str(read_xpt(tmp)), "Lorem ipsum dolor sit amet")
})

test_that("xportr_write: expect error when invalid multibyte string is passed in label", {
  expect_error(
    xportr_write(
      data_to_save,
      withr::local_file("xyz.xpt"),
      metadata = data.frame(
        dataset = "data_to_save",
        label = "Lorizzle ipsizzle dolizzl\xe7 pizzle"
      )
    )
  )
})

test_that("xportr_write: expect error when file name is over 8 characters long", {
  expect_error(
    xportr_write(
      data_to_save,
      withr::local_file(paste0(paste(letters[1:9], collapse = ""), ".xpt"))
    )
  )
})

test_that("xportr_write: expect error when file name contains non-ASCII symbols or special characters", {
  expect_error(
    xportr_write(data_to_save, withr::local_file("<test>.xpt"), strict_checks = TRUE)
  )
})

test_that("xportr_write: expect warning when file name contains underscore and strict_checks = FALSE", {
  expect_warning(
    xportr_write(data_to_save, withr::local_file("test_.xpt"), strict_checks = FALSE)
  )
})

test_that("xportr_write: expect error when label contains non-ASCII symbols or special characters", {
  expect_error(
    xportr_write(
      data_to_save,
      tmp,
      expect_error(
        xportr_write(
          data_to_save,
          domain = "data_to_save",
          path = withr::local_file("xyz.xpt"),
          metadata = data.frame(
            dataset = "data_to_save",
            label = "çtestç"
          )
        )
      )
    )
  )
})

test_that("xportr_write: expect error when label is over 40 characters", {
  expect_error(
    xportr_write(
      data_to_save,
      domain = "data_to_save",
      path = withr::local_file("xyz.xpt"),
      metadata = data.frame(
        dataset = "data_to_save",
        label = paste(rep("a", 41), collapse = "")
      )
    )
  )
})

test_that("xportr_write: expect error when an xpt validation fails with strict_checks set to TRUE", {
  attr(data_to_save$X, "format.sas") <- "foo"

  expect_error(
    xportr_write(
      data_to_save, withr::local_file("xyz.xpt"),
      domain = "data_to_save",
      metadata = data.frame(
        dataset = "data_to_save",
        label = "label"
      ),
      strict_checks = TRUE
    )
  )
})

test_that("xportr_write: expect warning when an xpt validation fails with strict_checks set to FALSE", {
  attr(data_to_save$X, "format.sas") <- "foo"

  expect_warning(
    xportr_write(
      data_to_save, withr::local_file("xyz.xpt"),
      domain = "data_to_save",
      metadata = data.frame(
        dataset = "data_to_save",
        label = "label"
      ),
      strict_checks = FALSE
    )
  )
})

test_that("xportr_write: Capture errors by haven and report them as such", {
  attr(data_to_save$X, "format.sas") <- "E8601LXw.asdf"

  expect_error(
    suppressWarnings(
      xportr_write(
        data_to_save, withr::local_file("xyz.xpt"),
        domain = "data_to_save",
        metadata = data.frame(
          dataset = "data_to_save",
          label = "label"
        ),
        strict_checks = FALSE
      )
    ),
    "Error reported by haven"
  )
})

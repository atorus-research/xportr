data_to_save <- function() minimal_table(cols = c("e", "b", "x")) %>% rename_with(toupper) %>% as_tibble()

test_that("xportr_write: exported data can be saved to a file", {
  skip_if_not_installed("withr")
  tmp <- withr::local_file("xyz.xpt")
  local_data <- data_to_save()

  xportr_write(local_data, path = tmp)
  expect_equal(read_xpt(tmp), local_data)
})

test_that("xportr_write: exported data can still be saved to a file with a label", {
  skip_if_not_installed("withr")
  tmp <- withr::local_file("xyz.xpt")

  suppressWarnings(
    xportr_write(data_to_save(),
      path = tmp,
      label = "Lorem ipsum dolor sit amet",
      domain = "data_to_save"
    )
  )
  expect_output(str(read_xpt(tmp)), "Lorem ipsum dolor sit amet")
})

test_that("xportr_write: exported data can be saved to a file with a metadata", {
  skip_if_not_installed("withr")
  tmp <- withr::local_file("xyz.xpt")

  xportr_write(
    data_to_save(),
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
  skip_if_not_installed("withr")
  tmp <- withr::local_file("xyz.xpt")

  df <- xportr_df_label(
    data_to_save(),
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
  skip_if_not_installed("withr")
  expect_error(
    xportr_write(
      data_to_save(),
      withr::local_file("xyz.xpt"),
      metadata = data.frame(
        dataset = "data_to_save",
        label = "Lorizzle ipsizzle dolizzl\xe7 pizzle"
      )
    )
  )
})

test_that("xportr_write: expect error when file name is over 8 characters long", {
  skip_if_not_installed("withr")
  expect_error(
    xportr_write(
      data_to_save(),
      withr::local_file(paste0(paste(letters[1:9], collapse = ""), ".xpt"))
    ),
    "`\\.df` file name must be 8 characters or less\\."
  )
})

test_that("xportr_write: expect error when file name contains non-ASCII symbols or special characters", {
  skip_if_not_installed("withr")
  expect_error(
    xportr_write(data_to_save(), withr::local_file("<test>.xpt"), strict_checks = TRUE),
    "`\\.df` cannot contain any non-ASCII, symbol or underscore characters\\."
  )
})

test_that("xportr_write: expect warning when file name contains underscore and strict_checks = FALSE", {
  skip_if_not_installed("withr")
  expect_warning(
    xportr_write(data_to_save(), withr::local_file("test_.xpt"), strict_checks = FALSE),
    "`\\.df` cannot contain any non-ASCII, symbol or underscore characters\\."
  )
})

test_that("xportr_write: expect error when label contains non-ASCII symbols or special characters", {
  skip_if_not_installed("withr")
  expect_error(
    xportr_write(
      data_to_save(),
      domain = "data_to_save",
      path = withr::local_file("xyz.xpt"),
      metadata = data.frame(
        dataset = "data_to_save",
        label = "çtestç"
      )
    ),
    "`label` cannot contain any non-ASCII, symbol or special characters"
  )
})

test_that("xportr_write: expect error when label is over 40 characters", {
  skip_if_not_installed("withr")
  expect_error(
    xportr_write(
      data_to_save(),
      domain = "data_to_save",
      path = withr::local_file("xyz.xpt"),
      metadata = data.frame(
        dataset = "data_to_save",
        label = paste(rep("a", 41), collapse = "")
      )
    ),
    "Length of dataset label must be 40 characters or less"
  )
})

test_that("xportr_write: expect error when an xpt validation fails with strict_checks set to TRUE", {
  skip_if_not_installed("withr")
  local_data <- data_to_save()
  attr(local_data$X, "format.sas") <- "foo"

  expect_error(
    xportr_write(
      local_data, withr::local_file("xyz.xpt"),
      domain = "data_to_save",
      metadata = data.frame(
        dataset = "data_to_save",
        label = "label"
      ),
      strict_checks = TRUE
    ),
    "Format 'X' must have a valid format\\."
  )
})

test_that("xportr_write: expect warning when an xpt validation fails with strict_checks set to FALSE", {
  skip_if_not_installed("withr")
  local_data <- data_to_save()
  attr(local_data$X, "format.sas") <- "foo"

  expect_warning(
    xportr_write(
      local_data, withr::local_file("xyz.xpt"),
      domain = "data_to_save",
      metadata = data.frame(
        dataset = "data_to_save",
        label = "label"
      ),
      strict_checks = FALSE
    ),
    "Format 'X' must have a valid format\\."
  )
})

test_that("xportr_write: Capture errors by haven and report them as such", {
  skip_if_not_installed("withr")
  local_data <- data_to_save()
  attr(local_data$X, "format.sas") <- "E8601LXw.asdf"

  expect_error(
    suppressWarnings(
      xportr_write(
        local_data, withr::local_file("xyz.xpt"),
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

data_to_save <- function() {
  minimal_table(cols = c("e", "b", "x")) %>%
    rename_with(toupper) %>%
    as_tibble()
}

# Skip large file tests unless explicitly requested
test_large_files <- Sys.getenv("XPORTR.TEST_LARGE_FILES", FALSE)

# xportr_write ----
## Test 1: xportr_write: exported data can be saved to a file ----
test_that("write Test 1: exported data can be saved to a file", {
  skip_if_not_installed("withr")
  tmp <- withr::local_file("xyz.xpt")
  local_data <- data_to_save()

  xportr_write(local_data, path = tmp)
  expect_equal(read_xpt(tmp), local_data)
})

## Test 2: xportr_write: exported data can still be saved to a file with a label ----
test_that("write Test 2: exported data can still be saved to a file with a label", {
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

## Test 3: xportr_write: exported data can be saved to a file with a metadata ----
test_that("write Test 3: exported data can be saved to a file with a metadata", {
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

## Test 4: xportr_write: exported data can be saved to a file with a existing metadata ----
test_that("write Test 4: exported data can be saved to a file with a existing metadata", {
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

## Test 5: xportr_write: expect error when invalid multibyte string is passed in label ----
test_that("write Test 5: expect error when invalid multibyte string is passed in label", {
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

## Test 6: xportr_write: expect error when file name is over 8 characters long ----
test_that("write Test 6: expect error when file name is over 8 characters long", {
  skip_if_not_installed("withr")
  expect_error(
    xportr_write(
      data_to_save(),
      withr::local_file(paste0(paste(letters[1:9], collapse = ""), ".xpt"))
    ),
    "\\.df file name must be 8 characters or less\\."
  )
})

## Test 7: xportr_write: expect error when file name contains non-ASCII symbols or special characters ----
test_that("write Test 7: expect error when file name contains non-ASCII symbols or special characters", {
  skip_if_not_installed("withr")
  expect_error(
    xportr_write(data_to_save(), withr::local_file("<test>.xpt"), strict_checks = TRUE),
    "`\\.df` cannot contain any non-ASCII, symbol or underscore characters\\."
  )
})

## Test 8: xportr_write: expect warning when file name contains underscore and strict_checks = FALSE ----
test_that("write Test 8: expect warning when file name contains underscore and strict_checks = FALSE", {
  skip_if_not_installed("withr")
  expect_warning(
    xportr_write(data_to_save(), withr::local_file("test_.xpt"), strict_checks = FALSE),
    "`\\.df` cannot contain any non-ASCII, symbol or underscore characters\\."
  )
})

## Test 9: xportr_write: expect error when label contains non-ASCII symbols or special characters ----
test_that("write Test 9: expect error when label contains non-ASCII symbols or special characters", {
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

## Test 10: xportr_write: expect error when label is over 40 characters ----
test_that("write Test 10: expect error when label is over 40 characters", {
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

## Test 11: xportr_write: expect error when an xpt validation fails with strict_checks set to TRUE ----
test_that("write Test 11: expect error when an xpt validation fails with strict_checks set to TRUE", {
  skip_if_not_installed("withr")
  local_data <- data_to_save()
  attr(local_data$X, "format.sas") <- "foo"

  expect_error(
    xportr_write(
      local_data,
      withr::local_file("xyz.xpt"),
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

## Test 12: xportr_write: expect warning when an xpt validation fails with strict_checks set to FALSE ----
test_that("write Test 12: expect warning when an xpt validation fails with strict_checks set to FALSE", {
  skip_if_not_installed("withr")
  local_data <- data_to_save()
  attr(local_data$X, "format.sas") <- "foo"

  expect_warning(
    xportr_write(
      local_data,
      withr::local_file("xyz.xpt"),
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

## Test 13: xportr_write: Capture errors by haven and report them as such ----
test_that("write Test 13: Capture errors by haven and report them as such", {
  skip_if_not_installed("withr")
  local_data <- data_to_save()
  attr(local_data$X, "format.sas") <- "E8601LXw.asdf"

  expect_error(
    suppressWarnings(
      xportr_write(
        local_data,
        withr::local_file("xyz.xpt"),
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

## Test 14: xportr_write: `split_by` attribute is used to split the data ----
test_that("write Test 14: `split_by` attribute is used to split the data", {
  tmpdir <- tempdir()
  tmp <- file.path(tmpdir, "xyz.xpt")

  on.exit(unlink(tmpdir))

  dts <- data_to_save()
  dts %>%
    xportr_split(split_by = "X") %>%
    xportr_write(path = tmp)

  expect_true(
    file.exists(file.path(tmpdir, "xyz1.xpt"))
  )
  expect_true(
    file.exists(file.path(tmpdir, "xyz2.xpt"))
  )
  expect_true(
    file.exists(file.path(tmpdir, "xyz3.xpt"))
  )
  expect_equal(
    read_xpt(file.path(tmpdir, "xyz1.xpt")) %>%
      extract2("X") %>%
      unique() %>%
      length(),
    1
  )
  expect_equal(
    read_xpt(file.path(tmpdir, "xyz2.xpt")) %>%
      extract2("X") %>%
      unique() %>%
      length(),
    1
  )
  expect_equal(
    read_xpt(file.path(tmpdir, "xyz3.xpt")) %>%
      extract2("X") %>%
      unique() %>%
      length(),
    1
  )
})

## Test 15: xportr_write: Large file sizes are reported and warned ----
test_that("write Test 15: Large file sizes are reported and warned", {
  skip_if_not(test_large_files)
  tmpdir <- tempdir()
  tmp <- file.path(tmpdir, "xyz.xpt")

  on.exit(unlink(tmpdir))

  # Large_df should be at least 5GB
  large_df <- do.call(
    data.frame, replicate(80000, rep("large", 80000), simplify = FALSE)
  )

  expect_warning(
    xportr_write(large_df, path = tmp),
    class = "xportr.xpt_size"
  )
})

data_to_save <- function() {
  minimal_table(cols = c("e", "b", "x")) %>%
    rename_with(toupper) %>%
    as_tibble()
}

# Skip large file tests unless explicitly requested
test_large_files <- Sys.getenv("XPORTR.TEST_LARGE_FILES", FALSE)

# xportr_write ----
## Test 1: exported data can be saved to a file ----
test_that("xportr_write Test 1: exported data can be saved to a file", {
  tmp <- withr::local_file("xyz.xpt")
  local_data <- data_to_save()

  xportr_write(local_data, path = tmp)
  expect_equal(read_xpt(tmp), local_data)
})

## Test 2: exported data can be saved to a file with a metadata ----
test_that("xportr_write Test 2: exported data can be saved to a file with a metadata", {
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

## Test 3: exported data can be saved to a file with a existing metadata ----
test_that("xportr_write Test 3: exported data can be saved to a file with a existing metadata", {
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

## Test 4: expect error when invalid multibyte string is passed in label ----
test_that("xportr_write Test 4: expect error when invalid multibyte string is passed in label", {
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

## Test 5: expect error when file name is over 8 characters long ----
test_that("xportr_write Test 5: expect error when file name is over 8 characters long", {
  expect_error(
    xportr_write(
      data_to_save(),
      withr::local_file(paste0(paste(letters[1:9], collapse = ""), ".xpt"))
    ),
    "\\.df file name must be 8 characters or less\\."
  )
})

## Test 6: expect error when file name contains non-ASCII symbols or special characters ----
test_that("xportr_write Test 6: expect error when file name contains non-ASCII symbols or special characters", {
  expect_error(
    xportr_write(data_to_save(), withr::local_file("<test>.xpt"), strict_checks = TRUE),
    "`\\.df` cannot contain any non-ASCII, symbol or underscore characters\\."
  )
})

## Test 7: expect warning when file name contains underscore and strict_checks = FALSE ----
test_that("xportr_write Test 7: expect warning when file name contains underscore and strict_checks = FALSE", {
  expect_warning(
    xportr_write(data_to_save(), withr::local_file("test_.xpt"), strict_checks = FALSE),
    "`\\.df` cannot contain any non-ASCII, symbol or underscore characters\\."
  )
})

## Test 8: expect error when label contains non-ASCII symbols or special characters ----
test_that("xportr_write Test 8: expect error when label contains non-ASCII symbols or special characters", {
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

## Test 9: expect error when label is over 40 characters ----
test_that("xportr_write Test 9: expect error when label is over 40 characters", {
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

## Test 10: expect error when an xpt validation fails with strict_checks set to TRUE ----
test_that("xportr_write Test 10: expect error when an xpt validation fails with strict_checks set to TRUE", {
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

## Test 11: expect warning when an xpt validation fails with strict_checks set to FALSE ----
test_that("xportr_write Test 11: expect warning when an xpt validation fails with strict_checks set to FALSE", {
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

## Test 12: Capture errors by haven and report them as such ----
test_that("xportr_write Test 12: Capture errors by haven and report them as such", {
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

## Test 13: if file size is less than `max_size_gb`, export only one file ----
test_that("xportr_write Test 13: if file size is less than `max_size_gb`, export only one file", {
  adlb <- pharmaverseadam::adlb

  tmpdir <- tempdir()

  # 5 GB
  max_size_gb <- 5

  expect_message(
    xportr_write(adlb,
      path = paste0(tmpdir, "/adlb.xpt"),
      domain = "adlb",
      max_size_gb = max_size_gb,
      strict_checks = FALSE
    ),
    "Data frame exported to 1 xpt files."
  )

  expect_true(
    file.exists(file.path(tmpdir, "adlb.xpt")),
    file.info(file.path(tmpdir, "adlb.xpt"))$size <= as.numeric(format(max_size_gb * 10^9, scientific = FALSE))
  )
})

## Test 14: `max_size_gb` is used to split data frame into specified maximum file size ----
test_that("xportr_write Test 14: `max_size_gb` is used to split data frame into specified maximum file size", {
  adlb <- pharmaverseadam::adlb

  tmpdir <- tempdir()

  # 20 mb
  max_size_gb <- 20 / 1000

  expect_message(
    xportr_write(adlb,
      path = paste0(tmpdir, "/adlb.xpt"),
      domain = "adlb",
      max_size_gb = max_size_gb,
      strict_checks = FALSE
    ),
    "Data frame exported to 5 xpt files."
  )

  expect_true(
    file.exists(file.path(tmpdir, "adlb1.xpt")),
    file.info(file.path(tmpdir, "adlb1.xpt"))$size <= as.numeric(format(max_size_gb * 10^9, scientific = FALSE))
  )

  expect_true(
    file.exists(file.path(tmpdir, "adlb2.xpt")),
    file.info(file.path(tmpdir, "adlb2.xpt"))$size <= as.numeric(format(max_size_gb * 10^9, scientific = FALSE))
  )

  expect_true(
    file.exists(file.path(tmpdir, "adlb3.xpt")),
    file.info(file.path(tmpdir, "adlb3.xpt"))$size <= as.numeric(format(max_size_gb * 10^9, scientific = FALSE))
  )

  expect_true(
    file.exists(file.path(tmpdir, "adlb4.xpt")),
    file.info(file.path(tmpdir, "adlb4.xpt"))$size <= as.numeric(format(max_size_gb * 10^9, scientific = FALSE))
  )

  expect_true(
    file.exists(file.path(tmpdir, "adlb5.xpt")),
    file.info(file.path(tmpdir, "adlb5.xpt"))$size <= as.numeric(format(max_size_gb * 10^9, scientific = FALSE))
  )
})


## Test 15: Large file sizes are reported and warned ----
test_that("xportr_write Test 15: Large file sizes are reported and warned", {
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

## Test 16: `split_by` attribute is used to split the data ----
test_that("xportr_write Test 16: `split_by` attribute is used to split the data", {
  tmpdir <- tempdir()
  tmp <- file.path(tmpdir, "xyz.xpt")

  on.exit(unlink(tmpdir))

  dts <- data_to_save()
  expect_warning(
    dts %>%
      xportr_split(split_by = "X") %>%
      xportr_write(path = tmp),
    class = "lifecycle_warning_deprecated"
  )

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

## Test 17: xportr_write warns about grouped data ----
test_that("xportr_write Test 17: xportr_write warns about grouped data", {
  tmp <- withr::local_file("test.xpt")
  local_data <- data_to_save()

  # Group the data
  grouped_data <- dplyr::group_by(local_data, E)

  # Verify data is grouped
  expect_true(dplyr::is_grouped_df(grouped_data))

  # xportr_write should warn about grouped data
  expect_warning(
    xportr_write(grouped_data, path = tmp),
    "Input data is grouped by:",
    fixed = FALSE
  )

  # Verify file was written successfully
  expect_true(file.exists(tmp))

  # Read back and verify data integrity (grouping should not affect the output)
  written_data <- read_xpt(tmp)
  expect_equal(nrow(written_data), nrow(local_data))
  expect_equal(ncol(written_data), ncol(local_data))
})

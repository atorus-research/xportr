## Test 1: utils-Get magrittr lhs side value ----
test_that("utils-xportr Test 1: utils-Get magrittr lhs side value", {
  x <- function(df, var) {
    get_pipe_call()
  }

  y <- function(df) {
    get_pipe_call()
  }

  expect_equal(
    {
      mtcars %>% x("cyl")
    },
    "mtcars"
  )
  expect_equal(
    {
      mtcars %>%
        subset(cyl == 6) %>%
        x("cyl")
    },
    "mtcars"
  )
})

# fmt_vars ----
## Test 2: utils-the message returns properly formatted variables ----
test_that("fmt_vars Test 2: utils-the message returns properly formatted variables", {
  expect_equal(fmt_vars(4), "Variable 4")
  expect_equal(fmt_vars(4:6), "Variables 4, 5, and 6")
})

## Test 3: utils-the message returns properly formatted labels ----
test_that("fmt_vars Test 3: utils-the message returns properly formatted labels", {
  expect_equal(fmt_labs(4), "Label '=4'")
  expect_equal(fmt_labs(4:6), "Labels '=4', '=5', and '=6'")
})

# xpt_validate_var_names ----
## Test 4: utils-Get error message when the variable is over 8 characters ----
test_that("xpt_validate_var_names Test 4: utils-Get error message when the variable is over 8 characters", {
  expect_equal(
    xpt_validate_var_names(c("FOO", "BAR", "ABCDEFGHI")),
    "Variable `ABCDEFGHI` must be 8 characters or less."
  )
})

## Test 5: utils-Get error message when the variable does not start with a letter ----
test_that("xpt_validate_var_names Test 5: utils-Get error message when the variable does not start with a letter", { # nolint
  expect_equal(
    xpt_validate_var_names(c("FOO", "2BAR")), # nolint
    "Variable `2BAR` must start with a letter."
  )
})

## Test 6: utils-Get error message when the variable contains non-ASCII characters or underscore ----
test_that("xpt_validate_var_names Test 6: utils-Get error message when the variable contains non-ASCII characters or underscore", { # nolint
  expect_equal(
    xpt_validate_var_names(c("FOO", "BAR", "FOO-BAR")),
    c(
      "Variable `FOO-BAR` cannot contain any non-ASCII, symbol or underscore characters.",
      "Variable `FOO-BAR` cannot contain any lowercase characters."
    )
  )
  expect_equal(
    xpt_validate_var_names(c("FOO", "BAR", "FOO_BAR")),
    c(
      "Variable `FOO_BAR` cannot contain any non-ASCII, symbol or underscore characters.",
      "Variable `FOO_BAR` cannot contain any lowercase characters."
    )
  )
})

## Test 7: utils-Get error message when the variable contains lowercase character ----
test_that("xpt_validate_var_names Test 7: utils-Get error message when the variable contains lowercase character", {
  xpt_validate_var_names(c("FOO", "bar"))
  expect_equal(
    xpt_validate_var_names(c("FOO", "bar")),
    "Variable `bar` cannot contain any lowercase characters."
  )
})

# xpt_validate ----
## Test 8: utils-Get error message when the label contains over 40 characters ----
test_that("xpt_validate Test 8: utils-Get error message when the label contains over 40 characters", {
  df <- data.frame(A = 1, B = 2)
  long_label <- paste(rep("a", 41), collapse = "")
  attr(df$A, "label") <- long_label
  expect_equal(
    xpt_validate(df),
    paste0("Label 'A=", long_label, "' must be 40 characters or less.")
  )
})

## Test 9: utils-Doesn't error out with iso8601 format ----
test_that("xpt_validate Test 9: utils-Doesn't error out with iso8601 format", {
  df <- data.frame(A = 1, B = 2)
  attr(df$A, "format.sas") <- "E8601LX."
  attr(df$B, "format.sas") <- "E8601DX20."
  expect_equal(
    xpt_validate(df),
    character(0)
  )
})

## Test 10: utils-Get error message when the label contains non-ASCII, symbol or special characters ----
test_that("xpt_validate Test 10: utils-Get error message when the label contains non-ASCII, symbol or special characters", { # nolint
  df <- data.frame(A = 1, B = 2)
  attr(df$A, "label") <- "fooçbar"
  expect_equal(
    xpt_validate(df),
    "Label 'A=fooçbar' cannot contain any non-ASCII, symbol or special characters."
  )
})

## Test 11: utils-Get error message when the length of a character variable is > 200 bytes  ----
test_that("xpt_validate Test 11: utils-Get error message when the length of a character variable is > 200 bytes ", {
  df <- data.frame(A = paste(rep("A", 201), collapse = ""))
  expect_equal(
    xpt_validate(df),
    "Length of A must be 200 bytes or less."
  )
})

## Test 12: utils-Get error message when the length of a non-ASCII character variable is > 200 bytes ----
test_that("xpt_validate Test 12: utils-Get error message when the length of a non-ASCII character variable is > 200 bytes", { # nolint
  df <- data.frame(A = paste(rep("一", 67), collapse = ""))
  expect_equal(
    xpt_validate(df),
    "Length of A must be 200 bytes or less."
  )
})

## Test 13: utils-Get error message when the length of a character variable is > 200 bytes and contains NAs ----
test_that("xpt_validate Test 13: utils-Get error message when the length of a character variable is > 200 bytes and contains NAs", { # nolint
  df <- data.frame(A = c(paste(rep("A", 201), collapse = ""), NA_character_))
  expect_equal(
    xpt_validate(df),
    "Length of A must be 200 bytes or less."
  )
})

## Test 14: group_data_check() warns and ungroups grouped data (verbose = 'warn') ----
test_that("xpt_validate Test 14: group_data_check() warns and ungroups grouped data (verbose = 'warn')", {
  df <- dplyr::group_by(mtcars, cyl)

  expect_true(dplyr::is_grouped_df(df))

  expect_warning(
    out <- xportr:::group_data_check(df, verbose = "warn"),
    "Input data is grouped by: cyl",
    fixed = TRUE
  )

  expect_false(dplyr::is_grouped_df(out))
  expect_identical(nrow(out), nrow(mtcars))
  expect_identical(names(out), names(mtcars))
})

## Test 15: group_data_check() messages and ungroups grouped data (verbose = 'message') ----
test_that("xpt_validate Test 15: group_data_check() messages and ungroups grouped data (verbose = 'message')", {
  df <- dplyr::group_by(mtcars, cyl)

  expect_message(
    out <- xportr:::group_data_check(df, verbose = "message"),
    "Input data is grouped by: cyl",
    fixed = TRUE
  )

  expect_false(dplyr::is_grouped_df(out))
  expect_identical(nrow(out), nrow(mtcars))
})

## Test 16: group_data_check() is fully silent and returns invisibly (verbose = 'quiet') ----
test_that("xpt_validate Test 16: group_data_check() is fully silent and returns invisibly (verbose = 'quiet')", {
  df <- dplyr::group_by(mtcars, cyl)

  # quiet => no warnings/messages; return invisible
  expect_silent(
    expect_invisible(
      out <- xportr:::group_data_check(df, verbose = "quiet")
    )
  )

  expect_false(dplyr::is_grouped_df(out))
  expect_identical(nrow(out), nrow(mtcars))
})

## Test 17: group_data_check() reports multiple grouping vars correctly ----
test_that("xpt_validate Test 17: group_data_check() reports multiple grouping vars correctly", {
  df <- dplyr::group_by(mtcars, cyl, gear)

  expect_warning(
    out <- xportr:::group_data_check(df, verbose = "warn"),
    "Input data is grouped by: cyl, gear",
    fixed = TRUE
  )

  expect_false(dplyr::is_grouped_df(out))
})

## Test 18: group_data_check() leaves ungrouped data unchanged and silent ----
test_that("xpt_validate Test 18: group_data_check() leaves ungrouped data unchanged and silent", {
  df <- mtcars

  expect_false(dplyr::is_grouped_df(df))

  expect_silent(
    out <- xportr:::group_data_check(df, verbose = "warn")
  )

  expect_identical(out, df)
  expect_false(dplyr::is_grouped_df(out))
})

## Test 19: group_data_check() errors on non-data-frame input (any verbose) ----
test_that("xpt_validate Test 19: group_data_check() errors on non-data-frame input (any verbose)", {
  expect_error(
    xportr:::group_data_check(1:5, verbose = "quiet")
  )

  expect_error(
    xportr:::group_data_check("not a df", verbose = "warn")
  )
})

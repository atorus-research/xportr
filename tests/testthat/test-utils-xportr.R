## Test 1: Get magrittr lhs side value ----
test_that("utils-xportr Test 1: Get magrittr lhs side value", {
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
## Test 2: fmt_vars: the message returns properly formatted variables ----
test_that("utils-xportr Test 2: the message returns properly formatted variables", {
  expect_equal(fmt_vars(4), "Variable 4")
  expect_equal(fmt_vars(4:6), "Variables 4, 5, and 6")
})

## Test 3: fmt_labs: the message returns properly formatted labels ----
test_that("utils-xportr Test 3: the message returns properly formatted labels", {
  expect_equal(fmt_labs(4), "Label '=4'")
  expect_equal(fmt_labs(4:6), "Labels '=4', '=5', and '=6'")
})

# xpt_validate_var_names ----
## Test 4: xpt_validate_var_names: Get error message when the variable is over 8 characters ----
test_that("utils-xportr Test 4: Get error message when the variable is over 8 characters", {
  expect_equal(
    xpt_validate_var_names(c("FOO", "BAR", "ABCDEFGHI")),
    "Variable `ABCDEFGHI` must be 8 characters or less."
  )
})

## Test 5: xpt_validate_var_names: Get error message when the variable does not start with a letter ----
test_that("utils-xportr Test 5: Get error message when the variable does not start with a letter", { # nolint
  expect_equal(
    xpt_validate_var_names(c("FOO", "2BAR")), # nolint
    "Variable `2BAR` must start with a letter."
  )
})

## Test 6: xpt_validate_var_names: Get error message when the variable contains non-ASCII characters or underscore ----
test_that("utils-xportr Test 6: Get error message when the variable contains non-ASCII characters or underscore", {
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

## Test 7: xpt_validate_var_names: Get error message when tje variable contains lowercase character ----
test_that("utils-xportr Test 7: Get error message when the variable contains lowercase character", {
  xpt_validate_var_names(c("FOO", "bar"))
  expect_equal(
    xpt_validate_var_names(c("FOO", "bar")),
    "Variable `bar` cannot contain any lowercase characters."
  )
})

# xpt_validate ----
## Test 8: xpt_validate: Get error message when the label contains over 40 characters ----
test_that("utils-xportr Test 8: Get error message when the label contains over 40 characters", {
  df <- data.frame(A = 1, B = 2)
  long_label <- paste(rep("a", 41), collapse = "")
  attr(df$A, "label") <- long_label
  expect_equal(
    xpt_validate(df),
    paste0("Label 'A=", long_label, "' must be 40 characters or less.")
  )
})

## Test 9: xpt_validate: Doesn't error out with iso8601 format ----
test_that("utils-xportr Test 9: Doesn't error out with iso8601 format", {
  df <- data.frame(A = 1, B = 2)
  attr(df$A, "format.sas") <- "E8601LX."
  attr(df$B, "format.sas") <- "E8601DX20."
  expect_equal(
    xpt_validate(df),
    character(0)
  )
})

## Test 10: xpt_validate: Get error message when the label contains non-ASCII, symbol or special characters ----
test_that("utils-xportr Test 10: Get error message when the label contains non-ASCII, symbol or special characters", {
  df <- data.frame(A = 1, B = 2)
  attr(df$A, "label") <- "fooçbar"
  expect_equal(
    xpt_validate(df),
    "Label 'A=fooçbar' cannot contain any non-ASCII, symbol or special characters."
  )
})

## Test 11: xpt_validate: Get error message when the length of a character variable is > 200 bytes  ----
test_that("utils-xportr Test 11: Get error message when the length of a character variable is > 200 bytes ", {
  df <- data.frame(A = paste(rep("A", 201), collapse = ""))
  expect_equal(
    xpt_validate(df),
    "Length of A must be 200 bytes or less."
  )
})

## Test 12: xpt_validate: Get error message when the length of a non-ASCII character variable is > 200 bytes ----
test_that("utils-xportr Test 12: Get error message when the length of a non-ASCII character variable is > 200 bytes", {
  df <- data.frame(A = paste(rep("一", 67), collapse = ""))
  expect_equal(
    xpt_validate(df),
    "Length of A must be 200 bytes or less."
  )
})

## Test 13: xpt_validate: Get error message when the length of a character variable is > 200 bytes and contains NAs ----
test_that("utils-xportr Test 13: Get error message when the length of a character variable is > 200 bytes and contains NAs", { # nolint
  df <- data.frame(A = c(paste(rep("A", 201), collapse = ""), NA_character_))
  expect_equal(
    xpt_validate(df),
    "Length of A must be 200 bytes or less."
  )
})

test_that("Get magrittr lhs side value", {
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


test_that("fmt_vars: the message returns properly formatted variables", {
  expect_equal(fmt_vars(4), "Variable 4")
  expect_equal(fmt_vars(4:6), "Variables 4, 5, and 6")
})

test_that("fmt_labs: the message returns properly formatted labels", {
  expect_equal(fmt_labs(4), "Label '=4'")
  expect_equal(fmt_labs(4:6), "Labels '=4', '=5', and '=6'")
})

test_that("xpt_validate_var_names: Get error message when the variable is over 8 characters", {
  expect_equal(
    xpt_validate_var_names(c("FOO", "BAR", "ABCDEFGHI")),
    "Variable `ABCDEFGHI` must be 8 characters or less."
  )
})

test_that("xpt_validate_var_names: Get error message when the variable does not start with a letter", {
  expect_equal(
    xpt_validate_var_names(c("FOO", "2BAR")),
    "Variable `2BAR` must start with a letter."
  )
})

test_that("xpt_validate_var_names: Get error message when the variable contains non-ASCII characters or underscore", {
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

test_that("xpt_validate_var_names: Get error message when tje variable contains lowercase character", {
  xpt_validate_var_names(c("FOO", "bar"))
  expect_equal(
    xpt_validate_var_names(c("FOO", "bar")),
    "Variable `bar` cannot contain any lowercase characters."
  )
})

test_that("xpt_validate: Get error message when the label contains over 40 characters", {
  df <- data.frame(A = 1, B = 2)
  long_label <- paste(rep("a", 41), collapse = "")
  attr(df$A, "label") <- long_label
  expect_equal(
    xpt_validate(df),
    paste0("Label 'A=", long_label, "' must be 40 characters or less.")
  )
})

test_that("xpt_validate: Get error message when the variable type is invalid", {
  df <- data.frame(A = 1, B = 2)
  attr(df$A, "SAStype") <- "integer"
  attr(df$B, "SAStype") <- "list"
  expect_equal(
    xpt_validate(df),
    "Variables `A` and `B` must have a valid type."
  )
})

test_that("xpt_validate: Doesn't error out with iso8601 format", {
  df <- data.frame(A = 1, B = 2)
  attr(df$A, "format.sas") <- "E8601LX."
  attr(df$B, "format.sas") <- "E8601DX20."
  expect_equal(
    xpt_validate(df),
    character(0)
  )
})

test_that("xpt_validate: Get error message when the label contains non-ASCII, symbol or special characters", {
  df <- data.frame(A = 1, B = 2)
  attr(df$A, "label") <- "fooçbar"
  expect_equal(
    xpt_validate(df),
    "Label 'A=fooçbar' cannot contain any non-ASCII, symbol or special characters."
  )
})

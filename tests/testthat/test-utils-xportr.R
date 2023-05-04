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

test_that("xpt_validate_var_names: ", {
  xpt_validate_var_names(c("A", "Bajskflas", "2klsd", "asdf_asdf"))
  expect_equal(1, 1)
})

test_that("xpt_validate", {
  df <- data.frame(A = 1, B = 2)
  attr(df$A, "label") <- "asdfkajsdkj_fhaksjdfkajshdfkajsdfkjhk<jashdfkjah"
  attr(df$B, "SAStype") <- "list"
  xpt_validate(df)
  expect_equal(1, 1)
})

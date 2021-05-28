test_that("Get magrittr lhs side value", {
  x <- function(df, var) {
    get_pipe_call()
  }
  
  y <- function(df) {
    get_pipe_call()
  }
  
  expect_equal({mtcars %>% x("cyl")}, "mtcars")
  expect_equal({mtcars %>%   subset(cyl == 6) %>%   x("cyl")}, "mtcars")
})

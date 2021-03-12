test_that("Get magrittr lhs side value", {
  x <- function(df, var) {
    magrittr_lhs()
  }
  
  y <- function(df) {
    magrittr_lhs()
  }
  
  expect_equal(x(mtcars, "cyl"), "mtcars")
  expect_equal(y(mtcars), "mtcars")
  expect_equal(mtcars %>% x("cyl"), "mtcars")
  expect_equal(mtcars %>% subset(cyl == 6) %>% x("cyl"), "mtcars")
})

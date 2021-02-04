library(tibble)

test_that("Variable label", {
  df <- tibble(x = "a", y = "b")
  varmeta <- tibble(dataset  = rep("df", 2), 
                    variable = c("x", "y"), 
                    label    = c("foo", "bar"))

  extract_varlabel <- function(.x) {
    vapply(.x, function(.x) attr(.x, "label"), character(1), USE.NAMES = FALSE) 
  }
  
  df <- define_varlabel(df, varmeta)
  
  expect_equal(extract_varlabel(df), c("foo", "bar"))
  expect_output(str(df), "1 x 2")
})

test_that("Dataset label", {
  df <- tibble(x = "a", y = "b")
  dfmeta <- tibble(name  = "df", 
                   label = "Label") 
  
  df <- define_dflabel(df, dfmeta)    
  expect_equal(attr(df, "label"), "Label") 
  expect_output(str(df), "1 x 2")
})

test_that("Expect error if any variable doesn't exist in var. metadata", {
  df <- tibble(x = "a", y = "b")
  varmeta <- tibble(dataset  = "df", 
                    variable = "x",
                    label    = "foo")
  
  expect_error(define_varlabel(df, varmeta), "present in input data but doesn't exist")
})

test_that("Expect error if any label exceeds 40 character", {
  df <- tibble(x = "a", y = "b")
  varmeta <- tibble(dataset  = rep("df", 2), 
                    variable = c("x", "y"), 
                    label    = c("foo", "Lorem ipsum dolor sit amet, consectetur adipiscing elit"))
  dfmeta <- tibble(name  = "df", 
                   label = "Lorem ipsum dolor sit amet, consectetur adipiscing elit")
  
  expect_error(define_varlabel(df, varmeta), "label exceeds 40 character")
  expect_error(define_dflabel(df, dfmeta), "label exceeds 40 character")
})



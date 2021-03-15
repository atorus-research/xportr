library(tibble)

test_that("Variable label", {
  df <- tibble(x = "a", y = "b")
  varmeta <- tibble(dataset  = rep("df", 2), 
                    variable = c("x", "y"), 
                    label    = c("foo", "bar"))
  
  extract_varlabel <- function(.x) {
    vapply(.x, function(.x) attr(.x, "label"), character(1), USE.NAMES = FALSE) 
  }
  
  df <- xportr_label(df, varmeta)
  
  expect_equal(extract_varlabel(df), c("foo", "bar"))
  expect_output(str(df), "1 x 2")
})

test_that("Dataset label", {
  df <- tibble(x = "a", y = "b")
  dfmeta <- tibble(name  = "df", 
                   label = "Label") 
  
  df <- xportr_df_label(df, dfmeta)    
  expect_equal(attr(df, "label"), "Label") 
  expect_output(str(df), "1 x 2")
})

test_that("Expect error if any variable doesn't exist in var. metadata", {
  df <- tibble(x = "a", y = "b")
  varmeta <- tibble(dataset  = "df", 
                    variable = "x",
                    label    = "foo")
  
  expect_error(xportr_label(df, varmeta), "present in `.df` but doesn't exist in `datadef`")
})

test_that("Expect error if any label exceeds 40 character", {
  df <- tibble(x = "a", y = "b")
  varmeta <- tibble(dataset  = rep("df", 2), 
                    variable = c("x", "y"), 
                    label    = c("foo", "Lorem ipsum dolor sit amet, consectetur adipiscing elit"))
  dfmeta <- tibble(name  = "df", 
                   label = "Lorem ipsum dolor sit amet, consectetur adipiscing elit")
  
  expect_error(xportr_label(df, varmeta), "variable label must be 40 characters or less")
  expect_error(xportr_df_label(df, dfmeta), "dataset label must be 40 characters or less")
})

test_that("SAS format", {
  df <- tibble(x = 1, y = 2)
  varmeta <- tibble(dataset  = rep("df", 2), 
                    variable = c("x", "y"), 
                    sas_format = c("date9.", "datetime20."))
  
  extract_format <- function(.x) {
    vapply(.x, function(.x) attr(.x, "SASformat"), character(1), USE.NAMES = FALSE) 
  }
  
  out <- xportr_format(df, varmeta)
  
  expect_equal(extract_format(out), c("DATE9.", "DATETIME20."))
  expect_output(str(out), "1 x 2")
})

test_that("Error ", {
  df1 <- tibble(x = 1, y = 2)
  df2 <- tibble(x = 3, y = 4)
  expect_error(xportr_label(df1, df2, domain = 1), 
               "`domain` must be a vector with type <character>.")
  expect_error(xportr_df_label(df1, df2, domain = mtcars), 
               "`domain` must be a vector with type <character>.")
  expect_error(xportr_format(df1, df2, domain = 1L), 
               "`domain` must be a vector with type <character>.")
})

# test_that("SAS length", {
#   df <- tibble(x = "a", y = "b")
#   varmeta <- tibble(dataset  = rep("df", 2), 
#                     variable = c("x", "y"), 
#                     type     = c("text", "text"),
#                     length   = c(1, 1))
#   
#   extract_length <- function(.x) {
#     vapply(.x, function(.x) attr(.x, "SASlength"), character(1), USE.NAMES = FALSE) 
#   }
#   
#   out <- define_length(df, varmeta)
#   
#   expect_equal(extract_length(out), c(1, 1))
#   expect_output(str(out), "1 x 2")
#   
#   df <- cbind(df, z = 3)
#   expect_error(define_length(df, varmeta), "doesn't exist")
# })
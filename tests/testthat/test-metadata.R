test_that("Variable label", {
  df <- data.frame(x = "a", y = "b")
  varmeta <- data.frame(dataset  = rep("df", 2), 
                    variable = c("x", "y"), 
                    label    = c("foo", "bar"))
  
  extract_varlabel <- function(.x) {
    vapply(.x, function(.x) attr(.x, "label"), character(1), USE.NAMES = FALSE) 
  }
  
  df <- xportr_label(df, varmeta)
  df_dput <- dput(df)
  
  expect_equal(extract_varlabel(df), c("foo", "bar"))
  expect_equal(df_dput,
                structure(list(x = structure("a", label = "foo"),
                               y = structure("b", label = "bar")),
                          row.names = c(NA, -1L), class = "data.frame"))
})

test_that("Dataset label", {
  df <- data.frame(x = "a", y = "b")
  dfmeta <- data.frame(dataset  = "df", 
                   label = "Label") 
  
  df <- xportr_df_label(df, dfmeta)    
  expect_equal(attr(df, "label"), "Label") 
  expect_equal(dput(df), structure(list(x = "a", y = "b"), class = "data.frame",
                                   row.names = c(NA, -1L), label = "Label"))
})

test_that("Expect error if any variable doesn't exist in var. metadata", {
  df <- data.frame(x = "a", y = "b")
  varmeta <- data.frame(dataset  = "df", 
                    variable = "x",
                    label    = "foo")
  
  # expect_error(xportr_label(df, varmeta, verbose = "stop"),
  #              "present in `.df` but doesn't exist in `datadef`")
})

test_that("Expect error if any label exceeds 40 character", {
  df <- data.frame(x = "a", y = "b")
  varmeta <- data.frame(dataset  = rep("df", 2), 
                    variable = c("x", "y"), 
                    label    = c("foo", "Lorem ipsum dolor sit amet, consectetur adipiscing elit"))
  dfmeta <- data.frame(dataset  = "df", 
                   label = "Lorem ipsum dolor sit amet, consectetur adipiscing elit")
  
  expect_warning(xportr_label(df, varmeta),
               "variable label must be 40 characters or less")
  expect_error(xportr_df_label(df, dfmeta),
               "dataset label must be 40 characters or less")
})

test_that("SAS format", {
  df <- data.frame(x = 1, y = 2)
  varmeta <- data.frame(dataset  = rep("df", 2), 
                    variable = c("x", "y"), 
                    format = c("date9.", "datetime20."))
  
  extract_format <- function(.x) {
    vapply(.x, function(.x) attr(.x, "format.sas"), character(1), USE.NAMES = FALSE) 
  }
  
  out <- xportr_format(df, varmeta)
  
  expect_equal(extract_format(out), c("DATE9.", "DATETIME20."))
  expect_equal(dput(out), structure(list(x = structure(1, format.sas = "DATE9."),
                                         y = structure(2, format.sas = "DATETIME20.")),
                                    row.names = c(NA, -1L), class = "data.frame"))
})

test_that("Error ", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- data.frame(x = 3, y = 4)
  expect_error(xportr_label(df1, df2, domain = 1), 
               "`domain` must be a vector with type <character>.")
  expect_error(xportr_df_label(df1, df2, domain = mtcars), 
               "`domain` must be a vector with type <character>.")
  expect_error(xportr_format(df1, df2, domain = 1L), 
               "`domain` must be a vector with type <character>.")
})

test_that("SAS length", {
  df <- data.frame(x = "a", y = "b")
  varmeta <- data.frame(dataset  = rep("df", 2),
                    variable = c("x", "y"),
                    type     = c("text", "text"),
                    length   = c(1, 1))

  extract_length <- function(.x) {
    vapply(.x, function(.x) attr(.x, "width"), character(1), USE.NAMES = FALSE)
  }

  out <- xportr_length(df, varmeta)

  expect_equal(c(x = 1, y = 1), map_dbl(out, attr, "width"))
  expect_equal(dput(out), structure(list(x = structure("a", width = 1),
                                          y = structure("b", width = 1)),
                                     row.names = c(NA, -1L), class = "data.frame"))

  df <- cbind(df, z = 3)
  expect_error(xportr_length(df, varmeta, verbose = "stop"), "doesn't exist")
})
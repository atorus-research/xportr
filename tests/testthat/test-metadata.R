suppressWarnings({
  library(metacore)
})

extract_format <- function(.x) {
  format_ <- character(length(.x))
  for (i in 1:length(.x)) {
    format_[i] <- attr(.x[[i]], "format.sas")
  }
  format_
}

test_that("Variable label", {
  df <- data.frame(x = "a", y = "b", variable = "value")
  df_meta <- data.frame(dataset  = rep("df", 2), variable = c("x", "y"), label = c("foo", "bar"))
  metacore_meta <- suppressWarnings(
    metacore(
      var_spec = data.frame(
        variable = c("x", "y"),
        type = "text",
        label = c("X Label", "Y Label"),
        length = c(4, 4),
        common = NA_character_,
        format = NA_character_
      )
    )
  )
  
  extract_var_label <- function(.x) {
    vapply(.x, function(.x) attr(.x, "label"), character(1), USE.NAMES = FALSE) 
  }
  
  df_labeled_df <- df %>% xportr_label(df_meta)
  metacoes_labeled_df <- df_labeled_df %>% xportr_label(metacore_meta, domain = "DOMAIN")
  
  expect_equal(extract_var_label(df_labeled_df), c("foo", "bar", ""))
  expect_equal(extract_var_label(metacoes_labeled_df), c("X Label", "Y Label", ""))
  expect_equal(
    dput(df_labeled_df),
    structure(
      list(
        x = structure("a", label = "foo"),
        y = structure("b", label = "bar"),
        variable = structure("value", label = "")
      ),
      row.names = c(NA, -1L),
      `_xportr.df_arg_` = "df",
      class = "data.frame"
    )
  )
  expect_equal(
    dput(metacoes_labeled_df),
    structure(
      list(
        x = structure("a", label = "X Label"),
        y = structure("b", label = "Y Label"),
        variable = structure("value", label = "")
      ),
      row.names = c(NA, -1L),
      `_xportr.df_arg_` = "DOMAIN",
      class = "data.frame"
    )
  )
})

test_that("Dataset label", {
  df <- data.frame(x = "a", y = "b")
  renamed_dataset_df <- structure(
    df,
    `_xportr.df_arg_` = "CUSTOMNAME"
  )

  df_meta <- data.frame(dataset  = "df", label = "Label")
  metacore_meta <- suppressWarnings(
    metacore(
      ds_spec = data.frame(
        dataset  = c("df", "CUSTOMNAME", "DOMAINNAME"),
        structure = "",
        label = c("Label", "Custom Label", "Domain Label")
      )
    )
  )
  
  df_spec_labeled_df <- xportr_df_label(df, df_meta)
  piped_spec_labeled_df <- df %>% xportr_df_label(df_meta)
  renamed_spec_labeled_df <- xportr_df_label(renamed_dataset_df, metacore_meta)
  domain_spec_labeled_df <- xportr_df_label(renamed_dataset_df, metacore_meta, domain = "DOMAINNAME")

  expect_equal(attr(df_spec_labeled_df, "label"), "Label") 
  expect_equal(
    dput(df_spec_labeled_df),
    structure(list(x = "a", y = "b"), class = "data.frame", row.names = c(NA, -1L), label = "Label")
  )
  expect_equal(attr(piped_spec_labeled_df, "label"), "Label") 
  expect_equal(
    dput(piped_spec_labeled_df),
    structure(
      list(x = "a", y = "b"), class = "data.frame",
      row.names = c(NA, -1L), `_xportr.df_arg_` = "df", label = "Label"
    )
  )
  expect_equal(attr(renamed_spec_labeled_df, "label"), "Custom Label") 
  expect_equal(
    dput(renamed_spec_labeled_df),
    structure(
      list(x = "a", y = "b"), class = "data.frame",
      row.names = c(NA, -1L), `_xportr.df_arg_` = "CUSTOMNAME", label = "Custom Label"
    )
  )
  expect_equal(attr(domain_spec_labeled_df, "label"), "Domain Label") 
  expect_equal(
    dput(domain_spec_labeled_df),
    structure(
      list(x = "a", y = "b"), class = "data.frame",
      row.names = c(NA, -1L), label = "Domain Label", `_xportr.df_arg_` = "DOMAINNAME"
    )
  )
})

test_that("Expect error if any variable doesn't exist in var. metadata", {
  df <- data.frame(x = "a", y = "b")
  varmeta <- data.frame(dataset  = "df", 
                    variable = "x",
                    label    = "foo")
  
  expect_error(xportr_label(df, varmeta, verbose = "stop"))
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

test_that("xportr_format will set formats as expected", {
  df <- data.frame(x = 1, y = 2)
  varmeta <- data.frame(dataset  = rep("df", 2), 
                        variable = c("x", "y"), 
                        format = c("date9.", "datetime20."))
  

  
  out <- xportr_format(df, varmeta)
  
  expect_equal(extract_format(out), c("DATE9.", "DATETIME20."))
  expect_equal(dput(out), structure(list(x = structure(1, format.sas = "DATE9."),
                                         y = structure(2, format.sas = "DATETIME20.")),
                                    row.names = c(NA, -1L), class = "data.frame"))
})

test_that("xportr_format will handle NA values and won't error", {
  df <- data.frame(x = 1, y = 2, z = 3, a = 4)
  varmeta <- data.frame(dataset  = rep("df", 4), 
                    variable = c("x", "y", "z", "abc"), 
                    format = c("date9.", "datetime20.", NA, "text"))
  
  out <- xportr_format(df, varmeta)
  
  expect_equal(extract_format(out), c("DATE9.", "DATETIME20.", "", ""))
  expect_equal(dput(out), structure(list(x = structure(1, format.sas = "DATE9."),
                                         y = structure(2, format.sas = "DATETIME20."),
                                         z = structure(3, format.sas = ""),
                                         a = structure(4, format.sas = "")),
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
suppressWarnings({
  library(metacore)
})

extract_format <- function(.x) {
  format_ <- character(length(.x))
  for (i in seq_along(.x)) {
    format_[i] <- attr(.x[[i]], "format.sas")
  }
  format_
}

extract_var_label <- function(.x) {
  vapply(.x, function(.x) attr(.x, "label"), character(1), USE.NAMES = FALSE)
}

test_that("xportr_label Test 1: correctly applies label for data.frame spec", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(dataset = rep("df", 2), variable = c("x", "y"), label = c("foo", "bar"))

  df_labeled_df <- xportr_label(df, df_meta)

  expect_equal(extract_var_label(df_labeled_df), c("foo", "bar"))
  expect_equal(
    dput(df_labeled_df),
    structure(
      list(
        x = structure("a", label = "foo"),
        y = structure("b", label = "bar")
      ),
      row.names = c(NA, -1L),
      class = "data.frame"
    )
  )
})

test_that("xportr_label Test 2: correctly applies label when the data is piped", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(dataset = rep("df", 2), variable = c("x", "y"), label = c("foo", "bar"))

  df_labeled_df <- df %>% xportr_label(df_meta)

  expect_equal(extract_var_label(df_labeled_df), c("foo", "bar"))
  expect_equal(
    dput(df_labeled_df),
    structure(
      list(
        x = structure("a", label = "foo"),
        y = structure("b", label = "bar")
      ),
      row.names = c(NA, -1L),
      `_xportr.df_arg_` = "df",
      class = "data.frame"
    )
  )
})

test_that("xportr_label Test 3: correctly applies label for custom domain", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(dataset = rep("DOMAIN", 2), variable = c("x", "y"), label = c("foo", "bar"))

  df_labeled_df <- xportr_label(df, df_meta, domain = "DOMAIN")

  expect_equal(extract_var_label(df_labeled_df), c("foo", "bar"))
  expect_equal(
    dput(df_labeled_df),
    structure(
      list(
        x = structure("a", label = "foo"),
        y = structure("b", label = "bar")
      ),
      row.names = c(NA, -1L),
      `_xportr.df_arg_` = "DOMAIN",
      class = "data.frame"
    )
  )
})

test_that("xportr_label Test 4: correctly applies label for metacore spec", {
  df <- data.frame(x = "a", y = "b", variable = "value")
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

  metacoes_labeled_df <- xportr_label(df, metacore_meta)

  expect_equal(extract_var_label(metacoes_labeled_df), c("X Label", "Y Label", ""))
  expect_equal(
    dput(metacoes_labeled_df),
    structure(
      list(
        x = structure("a", label = "X Label"),
        y = structure("b", label = "Y Label"),
        variable = structure("value", label = "")
      ),
      row.names = c(NA, -1L),
      class = "data.frame"
    )
  )
})

test_that("xportr_label Test 5: Expect error if any variable doesn't exist in metadata", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(
    dataset = "df",
    variable = "x",
    label = "foo"
  )

  expect_error(xportr_label(df, df_meta, verbose = "stop"))
})

test_that("xportr_label Test 6: Expect error if label exceeds 40 character", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(
    dataset = "df",
    variable = "x",
    label = strrep("a", 41)
  )

  expect_warning(
    xportr_label(df, df_meta),
    "variable label must be 40 characters or less"
  )
})

test_that("xportr_label Test 7: Expect error if domain is not a character", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(
    dataset = "df",
    variable = "x",
    label = "foo"
  )

  expect_error(
    xportr_label(df, df_meta, domain = 1),
    "`domain` must be a vector with type <character>."
  )
  expect_error(
    xportr_label(df, df_meta, domain = NA),
    "`domain` must be a vector with type <character>."
  )
})

test_that("xportr_df_label Test 1: correctly applies label for data.frame spec", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(dataset = "df", label = "Label")

  df_spec_labeled_df <- xportr_df_label(df, df_meta)

  expect_equal(attr(df_spec_labeled_df, "label"), "Label")
  expect_equal(
    dput(df_spec_labeled_df),
    structure(list(x = "a", y = "b"), class = "data.frame", row.names = c(NA, -1L), label = "Label")
  )
})

test_that("xportr_df_label Test 2: correctly applies label when the data is piped", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(dataset = "df", label = "Label")

  df_spec_labeled_df <- df %>% xportr_df_label(df_meta)

  expect_equal(attr(df_spec_labeled_df, "label"), "Label")
  expect_equal(
    dput(df_spec_labeled_df),
    structure(
      list(x = "a", y = "b"),
      class = "data.frame", row.names = c(NA, -1L), `_xportr.df_arg_` = "df", label = "Label"
    )
  )
})

test_that("xportr_df_label Test 3: correctly applies label for custom domain", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(dataset = "DOMAIN", label = "Label")

  df_spec_labeled_df <- xportr_df_label(df, df_meta, domain = "DOMAIN")

  expect_equal(attr(df_spec_labeled_df, "label"), "Label")
  expect_equal(
    dput(df_spec_labeled_df),
    structure(
      list(x = "a", y = "b"),
      class = "data.frame", row.names = c(NA, -1L), `_xportr.df_arg_` = "DOMAIN", label = "Label"
    )
  )
})

test_that("xportr_df_label Test 4: correctly applies label for metacore spec", {
  df <- data.frame(x = "a", y = "b")
  metacore_meta <- suppressWarnings(
    metacore(
      ds_spec = data.frame(
        dataset = c("df"),
        structure = "",
        label = c("Label")
      )
    )
  )

  metacore_spec_labeled_df <- xportr_df_label(df, metacore_meta)

  expect_equal(attr(metacore_spec_labeled_df, "label"), "Label")
  expect_equal(
    dput(metacore_spec_labeled_df),
    structure(
      list(x = "a", y = "b"),
      class = "data.frame",
      row.names = c(NA, -1L), label = "Label"
    )
  )
})

test_that("xportr_df_label Test 5: Expect error if label exceeds 40 character", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(
    dataset = "df",
    label = strrep("a", 41)
  )

  expect_error(
    xportr_df_label(df, df_meta),
    "dataset label must be 40 characters or less"
  )
})

test_that("xportr_df_label Test 6: Expect error if domain is not a character", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(
    dataset = "df",
    label = "foo"
  )

  expect_error(
    xportr_df_label(df, df_meta, domain = 1),
    "`domain` must be a vector with type <character>."
  )
  expect_error(
    xportr_df_label(df, df_meta, domain = NA),
    "`domain` must be a vector with type <character>."
  )
})

test_that("xportr_format will set formats as expected", {
  df <- data.frame(x = 1, y = 2)
  varmeta <- data.frame(
    dataset = rep("df", 2),
    variable = c("x", "y"),
    format = c("date9.", "datetime20.")
  )



  out <- xportr_format(df, varmeta)

  expect_equal(extract_format(out), c("DATE9.", "DATETIME20."))
  expect_equal(dput(out), structure(
    list(
      x = structure(1, format.sas = "DATE9."),
      y = structure(2, format.sas = "DATETIME20.")
    ),
    row.names = c(NA, -1L), class = "data.frame"
  ))
})

test_that("xportr_format will handle NA values and won't error", {
  df <- data.frame(x = 1, y = 2, z = 3, a = 4)
  varmeta <- data.frame(
    dataset = rep("df", 4),
    variable = c("x", "y", "z", "abc"),
    format = c("date9.", "datetime20.", NA, "text")
  )

  out <- xportr_format(df, varmeta)

  expect_equal(extract_format(out), c("DATE9.", "DATETIME20.", "", ""))
  expect_equal(dput(out), structure(
    list(
      x = structure(1, format.sas = "DATE9."),
      y = structure(2, format.sas = "DATETIME20."),
      z = structure(3, format.sas = ""),
      a = structure(4, format.sas = "")
    ),
    row.names = c(NA, -1L), class = "data.frame"
  ))
})

test_that("Error ", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- data.frame(x = 3, y = 4)

  expect_error(
    xportr_format(df1, df2, domain = 1L),
    "`domain` must be a vector with type <character>."
  )
})

test_that("SAS length", {
  df <- data.frame(x = "a", y = "b")
  varmeta <- data.frame(
    dataset = rep("df", 2),
    variable = c("x", "y"),
    type = c("text", "text"),
    length = c(1, 1)
  )

  extract_length <- function(.x) {
    vapply(.x, function(.x) attr(.x, "width"), character(1), USE.NAMES = FALSE)
  }

  out <- xportr_length(df, varmeta)

  expect_equal(c(x = 1, y = 1), map_dbl(out, attr, "width"))
  expect_equal(dput(out), structure(
    list(
      x = structure("a", width = 1),
      y = structure("b", width = 1)
    ),
    row.names = c(NA, -1L), class = "data.frame"
  ))

  df <- cbind(df, z = 3)
  expect_error(xportr_length(df, varmeta, verbose = "stop"), "doesn't exist")
})

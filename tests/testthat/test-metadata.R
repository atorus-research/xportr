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

test_that("xportr_label: Correctly applies label from data.frame spec", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(dataset = "df", variable = c("x", "y"), label = c("foo", "bar"))

  df_labeled_df <- xportr_label(df, df_meta)

  expect_equal(extract_var_label(df_labeled_df), c("foo", "bar"))

  expect_equal(
    df_labeled_df,
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

test_that("xportr_label: Correctly applies label when data is piped", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(dataset = "df", variable = c("x", "y"), label = c("foo", "bar"))

  df_labeled_df <- df %>% xportr_label(df_meta)

  expect_equal(extract_var_label(df_labeled_df), c("foo", "bar"))
  expect_equal(
    df_labeled_df,
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

test_that("xportr_label: Correctly applies label for custom domain", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(dataset = rep("DOMAIN", 2), variable = c("x", "y"), label = c("foo", "bar"))

  df_labeled_df <- xportr_label(df, df_meta, domain = "DOMAIN")

  expect_equal(extract_var_label(df_labeled_df), c("foo", "bar"))
  expect_equal(
    df_labeled_df,
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

test_that("xportr_label: Correctly applies label from metacore spec", {
  skip_if_not_installed("metacore")

  df <- data.frame(x = "a", y = "b", variable = "value")
  metacore_meta <- suppressMessages(suppressWarnings(
    metacore::metacore(
      var_spec = data.frame(
        variable = c("x", "y"),
        type = "text",
        label = c("X Label", "Y Label"),
        length = c(4, 4),
        common = NA_character_,
        format = NA_character_
      )
    )
  ))

  metacoes_labeled_df <- suppressMessages(
    xportr_label(df, metacore_meta)
  )

  expect_equal(extract_var_label(metacoes_labeled_df), c("X Label", "Y Label", ""))
  expect_equal(
    metacoes_labeled_df,
    structure(
      list(
        x = structure("a", label = "X Label"),
        y = structure("b", label = "Y Label"),
        variable = structure("value", label = "")
      ),
      row.names = c(NA, -1L),
      `_xportr.df_arg_` = "df",
      class = "data.frame"
    )
  )
})

test_that("xportr_label: Expect error if any variable does not exist in metadata", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(
    dataset = "df",
    variable = "x",
    label = "foo"
  )
  suppressMessages(
    xportr_label(df, df_meta, verbose = "stop")
  ) %>%
    expect_error()
})

test_that("xportr_label: Expect error if label exceeds 40 characters", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(
    dataset = "df",
    variable = "x",
    label = strrep("a", 41)
  )

  suppressMessages(xportr_label(df, df_meta)) %>%
    expect_warning("variable label must be 40 characters or less")
})

test_that("xportr_label: Expect error if domain is not a character", {
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

test_that("xportr_df_label: Correctly applies label from data.frame spec", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(dataset = "df", label = "Label")

  df_spec_labeled_df <- xportr_df_label(df, df_meta)

  expect_equal(attr(df_spec_labeled_df, "label"), "Label")
  expect_equal(
    df_spec_labeled_df,
    structure(
      list(x = "a", y = "b"),
      class = "data.frame",
      `_xportr.df_arg_` = "df",
      row.names = c(NA, -1L),
      label = "Label"
    )
  )
})

test_that("xportr_df_label: Correctly applies label when data is piped", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(dataset = "df", label = "Label")

  df_spec_labeled_df <- df %>%
    xportr_df_label(df_meta) %>%
    xportr_df_label(df_meta)

  expect_equal(attr(df_spec_labeled_df, "label"), "Label")
  expect_equal(
    df_spec_labeled_df,
    structure(
      list(x = "a", y = "b"),
      class = "data.frame", row.names = c(NA, -1L), `_xportr.df_arg_` = "df", label = "Label"
    )
  )
})

test_that("xportr_df_label: Correctly applies label for custom domain", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(dataset = "DOMAIN", label = "Label")

  df_spec_labeled_df <- xportr_df_label(df, df_meta, domain = "DOMAIN")

  expect_equal(attr(df_spec_labeled_df, "label"), "Label")
  expect_equal(
    df_spec_labeled_df,
    structure(
      list(x = "a", y = "b"),
      class = "data.frame", row.names = c(NA, -1L), `_xportr.df_arg_` = "DOMAIN", label = "Label"
    )
  )
})

test_that("xportr_df_label: Correctly applies label from metacore spec", {
  skip_if_not_installed("metacore")

  df <- data.frame(x = "a", y = "b")
  metacore_meta <- suppressMessages(suppressWarnings(
    metacore::metacore(
      ds_spec = data.frame(
        dataset = c("df"),
        structure = "",
        label = c("Label")
      )
    )
  ))

  metacore_spec_labeled_df <- xportr_df_label(df, metacore_meta)

  expect_equal(attr(metacore_spec_labeled_df, "label"), "Label")
  expect_equal(
    metacore_spec_labeled_df,
    structure(
      list(x = "a", y = "b"),
      class = "data.frame",
      `_xportr.df_arg_` = "df",
      row.names = c(NA, -1L), label = "Label"
    )
  )
})

test_that("xportr_df_label: Expect error if label exceeds 40 characters", {
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

test_that("xportr_df_label: Expect error if domain is not a character", {
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

test_that("xportr_format: Set formats as expected", {
  df <- data.frame(x = 1, y = 2)
  df_meta <- data.frame(
    dataset = "df",
    variable = c("x", "y"),
    format = c("date9.", "datetime20.")
  )

  formatted_df <- xportr_format(df, df_meta)

  expect_equal(extract_format(formatted_df), c("DATE9.", "DATETIME20."))
  expect_equal(formatted_df, structure(
    list(
      x = structure(1, format.sas = "DATE9."),
      y = structure(2, format.sas = "DATETIME20.")
    ),
    row.names = c(NA, -1L), `_xportr.df_arg_` = "df", class = "data.frame"
  ))
})

test_that("xportr_format: Set formats as expected when data is piped", {
  df <- data.frame(x = 1, y = 2)
  df_meta <- data.frame(
    dataset = "df",
    variable = c("x", "y"),
    format = c("date9.", "datetime20.")
  )

  formatted_df <- df %>% xportr_format(df_meta)

  expect_equal(extract_format(formatted_df), c("DATE9.", "DATETIME20."))
  expect_equal(formatted_df, structure(
    list(
      x = structure(1, format.sas = "DATE9."),
      y = structure(2, format.sas = "DATETIME20.")
    ),
    row.names = c(NA, -1L), `_xportr.df_arg_` = "df", class = "data.frame"
  ))
})

test_that("xportr_format: Set formats as expected for metacore spec", {
  skip_if_not_installed("metacore")
  df <- data.frame(x = 1, y = 2)
  metacore_meta <- suppressMessages(suppressWarnings(
    metacore::metacore(
      var_spec = data.frame(
        variable = c("x", "y"),
        type = "text",
        label = c("X Label", "Y Label"),
        length = c(1, 2),
        common = NA_character_,
        format = c("date9.", "datetime20.")
      )
    )
  ))

  formatted_df <- xportr_format(df, metacore_meta)

  expect_equal(extract_format(formatted_df), c("DATE9.", "DATETIME20."))
  expect_equal(formatted_df, structure(
    list(
      x = structure(1, format.sas = "DATE9."),
      y = structure(2, format.sas = "DATETIME20.")
    ),
    row.names = c(NA, -1L), `_xportr.df_arg_` = "df", class = "data.frame"
  ))
})

test_that("xportr_format: Set formats as expected for custom domain", {
  df <- data.frame(x = 1, y = 2)
  df_meta <- data.frame(
    dataset = "DOMAIN",
    variable = c("x", "y"),
    format = c("date9.", "datetime20.")
  )

  formatted_df <- xportr_format(df, df_meta, domain = "DOMAIN")

  expect_equal(extract_format(formatted_df), c("DATE9.", "DATETIME20."))
  expect_equal(formatted_df, structure(
    list(
      x = structure(1, format.sas = "DATE9."),
      y = structure(2, format.sas = "DATETIME20.")
    ),
    row.names = c(NA, -1L), `_xportr.df_arg_` = "DOMAIN", class = "data.frame"
  ))
})

test_that("xportr_format: Handle NA values without raising an error", {
  df <- data.frame(x = 1, y = 2, z = 3, a = 4)
  df_meta <- data.frame(
    dataset = rep("df", 4),
    variable = c("x", "y", "z", "abc"),
    format = c("date9.", "datetime20.", NA, "text")
  )

  formatted_df <- xportr_format(df, df_meta)

  expect_equal(extract_format(formatted_df), c("DATE9.", "DATETIME20.", "", ""))
  expect_equal(formatted_df, structure(
    list(
      x = structure(1, format.sas = "DATE9."),
      y = structure(2, format.sas = "DATETIME20."),
      z = structure(3, format.sas = ""),
      a = structure(4, format.sas = "")
    ),
    row.names = c(NA, -1L), `_xportr.df_arg_` = "df", class = "data.frame"
  ))
})

test_that("xportr_format: Expect error if domain is not a character", {
  df <- data.frame(x = 1, y = 2, z = 3, a = 4)
  df_meta <- data.frame(
    dataset = "df",
    variable = "x",
    format = c("date9.")
  )

  expect_error(
    xportr_format(df, df_meta, 1),
    "`domain` must be a vector with type <character>."
  )
  expect_error(
    xportr_format(df, df_meta, NA),
    "`domain` must be a vector with type <character>."
  )
})

test_that("xportr_length: Check if width attribute is set properly", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(
    dataset = "df",
    variable = c("x", "y"),
    type = c("text", "text"),
    length = c(1, 2)
  )

  df_with_width <- xportr_length(df, df_meta)

  expect_equal(c(x = 1, y = 2), map_dbl(df_with_width, attr, "width"))
  expect_equal(df_with_width, structure(
    list(
      x = structure("a", width = 1),
      y = structure("b", width = 2)
    ),
    row.names = c(NA, -1L), `_xportr.df_arg_` = "df", class = "data.frame"
  ))
})

test_that("xportr_length: Check if width attribute is set properly when data is piped", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(
    dataset = "df",
    variable = c("x", "y"),
    type = c("text", "text"),
    length = c(1, 2)
  )

  df_with_width <- df %>% xportr_length(df_meta)

  expect_equal(c(x = 1, y = 2), map_dbl(df_with_width, attr, "width"))
  expect_equal(df_with_width, structure(
    list(
      x = structure("a", width = 1),
      y = structure("b", width = 2)
    ),
    row.names = c(NA, -1L), `_xportr.df_arg_` = "df", class = "data.frame"
  ))
})

test_that("xportr_length: Check if width attribute is set properly for metacore spec", {
  skip_if_not_installed("metacore")
  df <- data.frame(x = "a", y = "b")
  metacore_meta <- suppressMessages(suppressWarnings(
    metacore::metacore(
      var_spec = data.frame(
        variable = c("x", "y"),
        type = "text",
        label = c("X Label", "Y Label"),
        length = c(1, 2),
        common = NA_character_,
        format = NA_character_
      )
    )
  ))

  df_with_width <- xportr_length(df, metacore_meta)

  expect_equal(c(x = 1, y = 2), map_dbl(df_with_width, attr, "width"))
  expect_equal(df_with_width, structure(
    list(
      x = structure("a", width = 1),
      y = structure("b", width = 2)
    ),
    row.names = c(NA, -1L), `_xportr.df_arg_` = "df", class = "data.frame"
  ))
})

test_that("xportr_length: Check if width attribute is set properly when custom domain is passed", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(
    dataset = rep("DOMAIN", 2),
    variable = c("x", "y"),
    type = c("text", "text"),
    length = c(1, 2)
  )

  df_with_width <- xportr_length(df, df_meta, domain = "DOMAIN")

  expect_equal(c(x = 1, y = 2), map_dbl(df_with_width, attr, "width"))
  expect_equal(df_with_width, structure(
    list(
      x = structure("a", width = 1),
      y = structure("b", width = 2)
    ),
    row.names = c(NA, -1L), `_xportr.df_arg_` = "DOMAIN", class = "data.frame"
  ))
})

test_that("xportr_length: Expect error when a variable is not present in metadata", {
  df <- data.frame(x = "a", y = "b", z = "c")
  df_meta <- data.frame(
    dataset = "df",
    variable = c("x", "y"),
    type = c("text", "text"),
    length = c(1, 2)
  )

  suppressMessages(
    xportr_length(df, df_meta, verbose = "stop")
  ) %>%
    expect_error("doesn't exist")
})

test_that("xportr_length: Check if length gets imputed when a new variable is passed", {
  df <- data.frame(x = "a", y = "b", z = 3)
  df_meta <- data.frame(
    dataset = "df",
    variable = "x",
    type = "text",
    length = 1
  )

  df_with_width <- suppressMessages(
    xportr_length(df, df_meta)
  )

  # 200 is the imputed length for character and 8 for other data types as in impute_length()
  expect_equal(c(x = 1, y = 200, z = 8), map_dbl(df_with_width, attr, "width"))
  expect_equal(df_with_width, structure(
    list(
      x = structure("a", width = 1),
      y = structure("b", width = 200),
      z = structure(3, width = 8)
    ),
    row.names = c(NA, -1L), `_xportr.df_arg_` = "df", class = "data.frame"
  ))
})

test_that("xportr_length: Expect error if domain is not a character", {
  df <- data.frame(x = "a", y = "b", z = 3)
  df_meta <- data.frame(
    dataset = "df",
    variable = "x",
    type = "text",
    length = 1
  )

  expect_error(
    xportr_length(df, df_meta, 1),
    "`domain` must be a vector with type <character>."
  )
  expect_error(
    xportr_length(df, df_meta, NA),
    "`domain` must be a vector with type <character>."
  )
})

# many tests here are more like qualification/domain testing - this section adds
# tests for `xportr_metadata()` basic functionality
# start
test_that("xportr_metadata: Check metadata interaction with other functions", {
  adsl <- admiral::admiral_adsl

  var_spec <-
    readxl::read_xlsx(
      system.file("specs", "ADaM_admiral_spec.xlsx", package = "xportr"),
      sheet = "Variables"
    ) %>%
    dplyr::rename(type = "Data Type") %>%
    rlang::set_names(tolower)

  expect_equal(
    structure(xportr_type(adsl, var_spec), `_xportr.df_metadata_` = var_spec),
    suppressMessages(
      xportr_metadata(adsl, var_spec) %>% xportr_type()
    )
  )

  expect_equal(
    structure(
      suppressMessages(xportr_length(adsl, var_spec)),
      `_xportr.df_metadata_` = var_spec
    ),
    suppressMessages(
      xportr_metadata(adsl, var_spec) %>% xportr_length()
    )
  )

  expect_equal(
    structure(
      suppressMessages(xportr_label(adsl, var_spec)),
      `_xportr.df_metadata_` = var_spec
    ),
    suppressMessages(
      xportr_metadata(adsl, var_spec) %>% xportr_label()
    )
  )

  expect_equal(
    structure(
      suppressMessages(xportr_order(adsl, var_spec)),
      `_xportr.df_metadata_` = var_spec
    ),
    suppressMessages(
      xportr_metadata(adsl, var_spec) %>% xportr_order()
    )
  )

  expect_equal(
    structure(
      suppressMessages(xportr_format(adsl, var_spec)),
      `_xportr.df_metadata_` = var_spec
    ),
    suppressMessages(
      xportr_metadata(adsl, var_spec) %>% xportr_format()
    )
  )
})

test_that("xportr_metadata: Correctly extract domain from var name", {
  metadata <- data.frame(
    dataset = "adlb",
    variable = c("Subj", "Param", "Val", "NotUsed"),
    type = c("numeric", "character", "numeric", "character"),
    order = c(1, 3, 4, 2)
  )

  adlb <- data.frame(
    Subj = as.character(123, 456, 789),
    Different = c("a", "b", "c"),
    Val = c("1", "2", "3"),
    Param = c("param1", "param2", "param3")
  )

  expect_equal(attr(xportr_metadata(adlb, metadata), "_xportr.df_arg_"), "adlb")
})
# end

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

# xportr_label ----
## Test 1: xportr_label: Correctly applies label from data.frame spec ----
test_that("metadata Test 1: Correctly applies label from data.frame spec", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(dataset = "df", variable = c("x", "y"), label = c("foo", "bar"))

  df_labeled_df <- xportr_label(df, df_meta, domain = "df")

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

## Test 2: xportr_label: Correctly applies label when data is piped ----
test_that("metadata Test 2: Correctly applies label when data is piped", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(dataset = "df", variable = c("x", "y"), label = c("foo", "bar"))

  df_labeled_df <- df %>% xportr_label(df_meta, domain = "df")

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

## Test 3: xportr_label: Correctly applies label for custom domain ----
test_that("metadata Test 3: Correctly applies label for custom domain", {
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

## Test 4: xportr_label: Correctly applies label from metacore spec ----
test_that("metadata Test 4: Correctly applies label from metacore spec", {
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
    xportr_label(df, metacore_meta, domain = "df")
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

## Test 5: xportr_label: Expect error if any variable does not exist in metadata ----
test_that("metadata Test 5: Expect error if any variable does not exist in metadata", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(
    dataset = "df",
    variable = "x",
    label = "foo"
  )
  suppressMessages(
    xportr_label(df, df_meta, verbose = "stop", domain = "df")
  ) %>%
    expect_error()
})

## Test 6: xportr_label: Expect error if label exceeds 40 characters ----
test_that("metadata Test 6: Expect error if label exceeds 40 characters", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(
    dataset = "df",
    variable = "x",
    label = strrep("a", 41)
  )

  suppressMessages(xportr_label(df, df_meta, domain = "df")) %>%
    expect_warning("variable label must be 40 characters or less")
})

## Test 7: xportr_label: Expect error if domain is not a character ----
test_that("metadata Test 7: Expect error if domain is not a character", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(
    dataset = "df",
    variable = "x",
    label = "foo"
  )

  expect_error(
    xportr_label(df, df_meta, domain = 1),
    "Assertion on 'domain' failed: Must be of type 'string' \\(or 'NULL'\\), not '.*'\\."
  )
  expect_error(
    xportr_label(df, df_meta, domain = NA),
    "Assertion on 'domain' failed: May not be NA\\."
  )
})

# xportr_df_label ----
## Test 8: xportr_df_label: Correctly applies label from data.frame spec ----
test_that("metadata Test 8: Correctly applies label from data.frame spec", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(dataset = "df", label = "Label")

  df_spec_labeled_df <- xportr_df_label(df, df_meta, domain = "df")

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

## Test 9: xportr_df_label: Correctly applies label when data is piped ----
test_that("metadata Test 9: Correctly applies label when data is piped", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(dataset = "df", label = "Label")

  df_spec_labeled_df <- df %>%
    xportr_metadata(domain = "df") %>%
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

## Test 10: xportr_df_label: Correctly applies label for custom domain ----
test_that("metadata Test 10: Correctly applies label for custom domain", {
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

## Test 11: xportr_df_label: Correctly applies label from metacore spec ----
test_that("metadata Test 11: Correctly applies label from metacore spec", {
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

  metacore_spec_labeled_df <- xportr_df_label(df, metacore_meta, domain = "df")

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

## Test 12: xportr_df_label: Expect error if label exceeds 40 characters ----
test_that("metadata Test 12: Expect error if label exceeds 40 characters", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(
    dataset = "df",
    label = strrep("a", 41)
  )

  expect_error(
    xportr_df_label(df, df_meta, domain = "df"),
    "dataset label must be 40 characters or less"
  )
})

## Test 13: xportr_df_label: Expect error if domain is not a character ----
test_that("metadata Test 13: Expect error if domain is not a character", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(
    dataset = "df",
    label = "foo"
  )

  expect_error(
    xportr_df_label(df, df_meta, domain = 1),
    "Assertion on 'domain' failed: Must be of type 'string' \\(or 'NULL'\\), not '.*'\\."
  )
  expect_error(
    xportr_df_label(df, df_meta, domain = NA),
    "Assertion on 'domain' failed: May not be NA\\."
  )
})

# xportr_format ----
## Test 14: xportr_format: Set formats as expected ----
test_that("metadata Test 14: Set formats as expected", {
  df <- data.frame(x = 1, y = 2)
  df_meta <- data.frame(
    dataset = "df",
    variable = c("x", "y"),
    format = c("date9.", "datetime20.")
  )

  formatted_df <- xportr_format(df, df_meta, domain = "df")

  expect_equal(extract_format(formatted_df), c("DATE9.", "DATETIME20."))
  expect_equal(formatted_df, structure(
    list(
      x = structure(1, format.sas = "DATE9."),
      y = structure(2, format.sas = "DATETIME20.")
    ),
    row.names = c(NA, -1L), `_xportr.df_arg_` = "df", class = "data.frame"
  ))
})

## Test 15: xportr_format: Set formats as expected when data is piped ----
test_that("metadata Test 15: Set formats as expected when data is piped", {
  df <- data.frame(x = 1, y = 2)
  df_meta <- data.frame(
    dataset = "df",
    variable = c("x", "y"),
    format = c("date9.", "datetime20.")
  )

  formatted_df <- df %>% xportr_format(df_meta, domain = "df")

  expect_equal(extract_format(formatted_df), c("DATE9.", "DATETIME20."))
  expect_equal(formatted_df, structure(
    list(
      x = structure(1, format.sas = "DATE9."),
      y = structure(2, format.sas = "DATETIME20.")
    ),
    row.names = c(NA, -1L), `_xportr.df_arg_` = "df", class = "data.frame"
  ))
})

## Test 16: xportr_format: Set formats as expected for metacore spec ----
test_that("metadata Test 16: Set formats as expected for metacore spec", {
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

  formatted_df <- xportr_format(df, metacore_meta, domain = "df")

  expect_equal(extract_format(formatted_df), c("DATE9.", "DATETIME20."))
  expect_equal(formatted_df, structure(
    list(
      x = structure(1, format.sas = "DATE9."),
      y = structure(2, format.sas = "DATETIME20.")
    ),
    row.names = c(NA, -1L), `_xportr.df_arg_` = "df", class = "data.frame"
  ))
})

## Test 17: xportr_format: Set formats as expected for custom domain ----
test_that("metadata Test 17: Set formats as expected for custom domain", {
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

## Test 18: xportr_format: Handle NA values without raising an error ----
test_that("metadata Test 18: Handle NA values without raising an error", {
  df <- data.frame(x = 1, y = 2, z = 3, a = 4)
  df_meta <- data.frame(
    dataset = rep("df", 4),
    variable = c("x", "y", "z", "abc"),
    format = c("date9.", "datetime20.", NA, "text")
  )

  formatted_df <- xportr_format(df, df_meta, domain = "df")

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

## Test 19: xportr_format: Expect error if domain is not a character ----
test_that("metadata Test 19: Expect error if domain is not a character", {
  df <- data.frame(x = 1, y = 2, z = 3, a = 4)
  df_meta <- data.frame(
    dataset = "df",
    variable = "x",
    format = c("date9.")
  )

  expect_error(
    xportr_format(df, df_meta, 1),
    "Assertion on 'domain' failed: Must be of type 'string' \\(or 'NULL'\\), not '.*'\\."
  )
  expect_error(
    xportr_format(df, df_meta, NA),
    "Assertion on 'domain' failed: May not be NA\\."
  )
})

# xportr_length ----
## Test 20: xportr_length: Check if width attribute is set properly ----
test_that("metadata Test 20: Check if width attribute is set properly", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(
    dataset = "df",
    variable = c("x", "y"),
    type = c("text", "text"),
    length = c(1, 2)
  )

  df_with_width <- xportr_length(df, df_meta, domain = "df")

  expect_equal(c(x = 1, y = 2), map_dbl(df_with_width, attr, "width"))
  expect_equal(df_with_width, structure(
    list(
      x = structure("a", width = 1),
      y = structure("b", width = 2)
    ),
    row.names = c(NA, -1L), `_xportr.df_arg_` = "df", class = "data.frame"
  ))
})

## Test 21: xportr_length: Check if width attribute is set properly when data is piped ----
test_that("metadata Test 21: Check if width attribute is set properly when data is piped", {
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(
    dataset = "df",
    variable = c("x", "y"),
    type = c("text", "text"),
    length = c(1, 2)
  )

  df_with_width <- df %>% xportr_length(df_meta, domain = "df")

  expect_equal(c(x = 1, y = 2), map_dbl(df_with_width, attr, "width"))
  expect_equal(df_with_width, structure(
    list(
      x = structure("a", width = 1),
      y = structure("b", width = 2)
    ),
    row.names = c(NA, -1L), `_xportr.df_arg_` = "df", class = "data.frame"
  ))
})

## Test 22: xportr_length: Check if width attribute is set properly for metacore spec ----
test_that("metadata Test 22: Check if width attribute is set properly for metacore spec", {
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

  df_with_width <- xportr_length(df, metacore_meta, domain = "df")

  expect_equal(c(x = 1, y = 2), map_dbl(df_with_width, attr, "width"))
  expect_equal(df_with_width, structure(
    list(
      x = structure("a", width = 1),
      y = structure("b", width = 2)
    ),
    row.names = c(NA, -1L), `_xportr.df_arg_` = "df", class = "data.frame"
  ))
})

## Test 23: xportr_length: Check if width attribute is set properly when custom domain is passed ----
test_that("metadata Test 23: Check if width attribute is set properly when custom domain is passed", {
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

## Test 24: xportr_length: Expect error when a variable is not present in metadata ----
test_that("metadata Test 24: Expect error when a variable is not present in metadata", {
  df <- data.frame(x = "a", y = "b", z = "c")
  df_meta <- data.frame(
    dataset = "df",
    variable = c("x", "y"),
    type = c("text", "text"),
    length = c(1, 2)
  )

  suppressMessages(
    xportr_length(df, df_meta, domain = "df", verbose = "stop")
  ) %>%
    expect_error("doesn't exist")
})

## Test 25: xportr_length: Check if length gets imputed when a new variable is passed ----
test_that("metadata Test 25: Check if length gets imputed when a new variable is passed", {
  df <- data.frame(x = "a", y = "b", z = 3)
  df_meta <- data.frame(
    dataset = "df",
    variable = "x",
    type = "text",
    length = 1
  )

  df_with_width <- suppressMessages(
    xportr_length(df, df_meta, domain = "df")
  )

  # Max length is the imputed length for character and 8 for other data types
  expect_equal(c(x = 1, y = 1, z = 8), map_dbl(df_with_width, attr, "width"))
  expect_equal(df_with_width, structure(
    list(
      x = structure("a", width = 1),
      y = structure("b", width = 1),
      z = structure(3, width = 8)
    ),
    row.names = c(NA, -1L), `_xportr.df_arg_` = "df", class = "data.frame"
  ))
})

## Test 26: xportr_length: Expect error if domain is not a character ----
test_that("metadata Test 26: Expect error if domain is not a character", {
  df <- data.frame(x = "a", y = "b", z = 3)
  df_meta <- data.frame(
    dataset = "df",
    variable = "x",
    type = "text",
    length = 1
  )

  expect_error(
    xportr_length(df, df_meta, 1),
    "Assertion on 'domain' failed: Must be of type 'string' \\(or 'NULL'\\), not '.*'\\."
  )
  expect_error(
    xportr_length(df, df_meta, NA),
    "Assertion on 'domain' failed: May not be NA\\."
  )
})

# xportr_metadata ----
## Test 27: xportr_metadata: Impute character lengths based on class ----
test_that("metadata Test 27: Impute character lengths based on class", {
  adsl <- minimal_table(30, cols = c("x", "b"))
  metadata <- minimal_metadata(
    dataset = TRUE, length = TRUE, var_names = colnames(adsl)
  ) %>%
    mutate(length = length - 1)

  adsl <- adsl %>%
    mutate(
      new_date = as.Date(.data$x, origin = "1970-01-01"),
      new_char = as.character(.data$b),
      new_num = as.numeric(.data$x)
    )

  adsl %>%
    xportr_metadata(metadata, verbose = "none") %>%
    xportr_length() %>%
    expect_message("Variable lengths missing from metadata") %>%
    expect_message("lengths resolved") %>%
    expect_attr_width(c(7, 199, 200, 200, 8))
})

## Test 28: xportr_metadata: Throws message when variables not present in metadata ----
test_that("metadata Test 28: Throws message when variables not present in metadata", {
  adsl <- minimal_table(30, cols = c("x", "y"))
  metadata <- minimal_metadata(dataset = TRUE, length = TRUE, var_names = c("x"))

  # Test that message is given which indicates that variable is not present
  xportr_metadata(adsl, metadata, verbose = "message") %>%
    xportr_length() %>%
    expect_message("Variable lengths missing from metadata") %>%
    expect_message("lengths resolved") %>%
    expect_message(regexp = "Problem with `y`")
})

## Test 29: xportr_metadata: Variable ordering messaging is correct ----
test_that("metadata Test 29: Variable ordering messaging is correct", {
  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])
  df2 <- data.frame(a = "a", z = "z")
  df_meta <- data.frame(
    dataset = "df",
    variable = letters[1:4],
    order = 1:4
  )

  # Metadata versions
  xportr_metadata(df, df_meta, domain = "df", verbose = "message") %>%
    xportr_order() %>%
    expect_message("All variables in specification file are in dataset") %>%
    expect_condition("4 reordered in dataset") %>%
    expect_message("Variable reordered in `.df`: `a`, `b`, `c`, and `d`")

  xportr_metadata(df2, df_meta, domain = "df2", verbose = "message") %>%
    xportr_order() %>%
    expect_message("2 variables not in spec and moved to end") %>%
    expect_message("Variable moved to end in `.df`: `a` and `z`") %>%
    expect_message("All variables in dataset are ordered")
})

# xportr_type ----
## Test 30: xportr_type: Variable types are coerced as expected and can raise messages ----
test_that("metadata Test 30:  Variable types are coerced as expected and can raise messages", {
  df <- data.frame(
    Subj = as.character(c(123, 456, 789, "", NA, NA_integer_)),
    Different = c("a", "b", "c", "", NA, NA_character_),
    Val = c("1", "2", "3", "", NA, NA_character_),
    Param = c("param1", "param2", "param3", "", NA, NA_character_)
  )
  meta_example <- data.frame(
    dataset = "df",
    variable = c("Subj", "Param", "Val", "NotUsed"),
    type = c("numeric", "character", "numeric", "character"),
    format = NA
  )

  # Metadata version of the last statement
  df %>%
    xportr_metadata(meta_example, domain = "df", verbose = "warn") %>%
    xportr_type() %>%
    expect_warning()

  # Metadata version
  df %>%
    xportr_metadata(meta_example, domain = "df", verbose = "message") %>%
    xportr_type() %>%
    expect_message("Variable type\\(s\\) in dataframe don't match metadata")
})

# xportr_metadata ----
# many tests here are more like qualification/domain testing - this section adds
# tests for `xportr_metadata()` basic functionality
# start
## Test 31: xportr_metadata: Check metadata interaction with other functions ----
test_that("metadata Test 31: Check metadata interaction with other functions", {
  data("adsl_xportr", envir = environment())
  adsl <- adsl_xportr

  var_spec <- readxl::read_xlsx(
    system.file("specs", "ADaM_spec.xlsx", package = "xportr"),
    sheet = "Variables"
  ) %>%
    dplyr::rename(type = "Data Type") %>%
    dplyr::rename_with(tolower)

  # Divert all messages to tempfile, instead of printing them
  #  note: be aware as this should only be used in tests that don't track
  #        messages
  if (requireNamespace("withr", quiet = TRUE)) {
    withr::local_message_sink(withr::local_tempfile())
  }
  expect_equal(
    structure(xportr_type(adsl, var_spec, domain = "adsl"),
      `_xportr.df_metadata_` = var_spec,
      `_xportr.df_verbose_` = "none"
    ),
    suppressMessages(
      xportr_metadata(adsl, var_spec, domain = "adsl", verbose = "none") %>%
        xportr_type()
    )
  )

  expect_equal(
    structure(xportr_length(adsl, var_spec, domain = "adsl"),
      `_xportr.df_metadata_` = var_spec,
      `_xportr.df_verbose_` = "none"
    ),
    suppressMessages(
      xportr_metadata(adsl, var_spec, domain = "adsl", verbose = "none") %>%
        xportr_length()
    )
  )

  expect_equal(
    structure(xportr_label(adsl, var_spec, domain = "adsl"),
      `_xportr.df_metadata_` = var_spec,
      `_xportr.df_verbose_` = "none"
    ),
    suppressMessages(
      xportr_metadata(adsl, var_spec, domain = "adsl", verbose = "none") %>%
        xportr_label()
    )
  )

  expect_equal(
    structure(xportr_order(adsl, var_spec, domain = "adsl"),
      `_xportr.df_metadata_` = var_spec,
      `_xportr.df_verbose_` = "none"
    ),
    suppressMessages(
      xportr_metadata(adsl, var_spec, domain = "adsl", verbose = "none") %>%
        xportr_order()
    )
  )

  expect_equal(
    structure(xportr_format(adsl, var_spec, domain = "adsl"),
      `_xportr.df_metadata_` = var_spec,
      `_xportr.df_verbose_` = "none"
    ),
    suppressMessages(
      xportr_metadata(adsl, var_spec, domain = "adsl", verbose = "none") %>%
        xportr_format()
    )
  )
})

## Test 32: xportr_metadata: must throw error if both metadata and domain are null ----
test_that("metadata Test 32: must throw error if both metadata and domain are null", {
  expect_error(
    xportr_metadata(data.frame(), metadata = NULL, domain = NULL),
    "Must provide either `metadata` or `domain` argument"
  )
})

# xportr_* ----
## Test 33: xportr_*: Domain is kept in between calls ----
test_that("metadata Test 33: Domain is kept in between calls", {
  # Divert all messages to tempfile, instead of printing them
  #  note: be aware as this should only be used in tests that don't track
  #        messages
  if (requireNamespace("withr", quietly = TRUE)) {
    withr::local_message_sink(withr::local_tempfile())
  }

  adsl <- minimal_table(30)

  metadata <- minimal_metadata(
    dataset = TRUE, length = TRUE, label = TRUE, type = TRUE, format = TRUE,
    order = TRUE
  )

  df2 <- adsl %>%
    xportr_metadata(domain = "adsl") %>%
    xportr_type(metadata)

  df3 <- df2 %>%
    xportr_label(metadata) %>%
    xportr_length(metadata) %>%
    xportr_order(metadata) %>%
    xportr_format(metadata)

  expect_equal(attr(df3, "_xportr.df_arg_"), "adsl")

  df4 <- adsl %>%
    xportr_type(metadata, domain = "adsl")

  df5 <- df4 %>%
    xportr_label(metadata) %>%
    xportr_length(metadata) %>%
    xportr_order(metadata) %>%
    xportr_format(metadata)

  expect_equal(attr(df5, "_xportr.df_arg_"), "adsl")
})
# end

# `xportr_metadata()` ----
## Test 34: `xportr_metadata()` results match traditional results ----
test_that("metadata Test 34: results match traditional results", {
  data("var_spec", "dataset_spec", "adsl_xportr", envir = environment())
  adsl <- adsl_xportr

  skip_if_not_installed("withr")
  trad_path <- withr::local_file("adsltrad.xpt")
  metadata_path <- withr::local_file("adslmeta.xpt")

  dataset_spec_low <- setNames(dataset_spec, tolower(names(dataset_spec)))
  names(dataset_spec_low)[[2]] <- "label"

  var_spec_low <- setNames(var_spec, tolower(names(var_spec)))
  names(var_spec_low)[[5]] <- "type"

  metadata_df <- adsl %>%
    xportr_metadata(var_spec_low, "ADSL", verbose = "none") %>%
    xportr_type() %>%
    xportr_length() %>%
    xportr_label() %>%
    xportr_order() %>%
    xportr_format() %>%
    xportr_df_label(dataset_spec_low) %>%
    xportr_write(metadata_path)

  trad_df <- adsl %>%
    xportr_type(var_spec_low, "ADSL", verbose = "none") %>%
    xportr_length(var_spec_low, "ADSL", verbose = "none") %>%
    xportr_label(var_spec_low, "ADSL", verbose = "none") %>%
    xportr_order(var_spec_low, "ADSL", verbose = "none") %>%
    xportr_format(var_spec_low, "ADSL") %>%
    xportr_df_label(dataset_spec_low, "ADSL") %>%
    xportr_write(trad_path)

  expect_identical(
    metadata_df,
    structure(
      trad_df,
      `_xportr.df_metadata_` = var_spec_low,
      `_xportr.df_verbose_` = "none"
    )
  )

  expect_identical(
    read_xpt(metadata_path),
    read_xpt(trad_path)
  )
})

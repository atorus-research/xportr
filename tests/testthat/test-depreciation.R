test_that("xportr_df_label: deprecated metacore argument still works and gives warning", {
  withr::local_options(lifecycle_verbosity = "quiet")
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(dataset = "df", label = "Label")

  df_spec_labeled_df <- xportr_df_label(df, metacore = df_meta)

  expect_equal(attr(df_spec_labeled_df, "label"), "Label")
  xportr_df_label(df, metacore = df_meta) %>%
    lifecycle::expect_deprecated("Please use the `metadata` argument instead.")
})

test_that("xportr_format: deprecated metacore argument still works and gives warning", {
  withr::local_options(lifecycle_verbosity = "quiet")
  df <- data.frame(x = 1, y = 2)
  df_meta <- data.frame(
    dataset = "df",
    variable = "x",
    format = "date9."
  )

  formatted_df <- xportr_format(df, metacore = df_meta)

  expect_equal(attr(formatted_df$x, "format.sas"), "DATE9.")
  xportr_format(df, metacore = df_meta) %>%
    lifecycle::expect_deprecated("Please use the `metadata` argument instead.")
})

test_that("xportr_label: deprecated metacore argument still works and gives warning", {
  withr::local_options(lifecycle_verbosity = "quiet")

  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(dataset = "df", variable = "x", label = "foo")

  df_labeled_df <- suppressMessages(
    xportr_label(df, metacore = df_meta)
  )

  expect_equal(attr(df_labeled_df$x, "label"), "foo")

  # Note that only the deprecated message should be caught (others are ignored)
  suppressMessages(
    xportr_label(df, metacore = df_meta) %>%
      lifecycle::expect_deprecated("Please use the `metadata` argument instead.")
  )
})

test_that("xportr_length: deprecated metacore argument still works and gives warning", {
  withr::local_options(lifecycle_verbosity = "quiet")
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(
    dataset = "df",
    variable = c("x", "y"),
    type = c("text", "text"),
    length = c(1, 2)
  )

  df_with_width <- xportr_length(df, metacore = df_meta)

  expect_equal(c(x = 1, y = 2), map_dbl(df_with_width, attr, "width"))

  xportr_length(df, metacore = df_meta) %>%
    lifecycle::expect_deprecated("Please use the `metadata` argument instead.")
})

test_that("xportr_order: deprecated metacore argument still works and gives warning", {
  withr::local_options(lifecycle_verbosity = "quiet")

  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])
  df_meta <- data.frame(
    dataset = "DOMAIN",
    variable = letters[1:4],
    order = 1:4
  )

  ordered_df <- suppressMessages(
    xportr_order(df, metacore = df_meta, domain = "DOMAIN")
  )

  expect_equal(names(ordered_df), df_meta$variable)

  # Note that only the deprecated message should be caught (others are ignored)
  suppressMessages(
    xportr_order(df, metacore = df_meta) %>%
      lifecycle::expect_deprecated("Please use the `metadata` argument instead.")
  )
})

test_that("xportr_type: deprecated metacore argument still works and gives warning", {
  withr::local_options(lifecycle_verbosity = "quiet")
  df <- data.frame(
    Subj = as.character(c(123, 456, 789, "", NA, NA_integer_)),
    Different = c("a", "b", "c", "", NA, NA_character_),
    Val = c("1", "2", "3", "", NA, NA_character_),
    Param = c("param1", "param2", "param3", "", NA, NA_character_)
  )
  df_meta <- data.frame(
    dataset = "df",
    variable = c("Subj", "Param", "Val", "NotUsed"),
    type = c("numeric", "character", "numeric", "character"),
    format = NA
  )

  df2 <- suppressMessages(
    xportr_type(df, metacore = df_meta)
  )

  # Note that only the deprecated message should be caught (others are ignored)
  suppressMessages(
    xportr_type(df, metacore = df_meta) %>%
      lifecycle::expect_deprecated("Please use the `metadata` argument instead.")
  )
})

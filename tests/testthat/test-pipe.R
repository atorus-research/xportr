test_that("xportr_*: Domain is obtained from a call without pipe", {
  adsl <- minimal_table(30)

  metadata <- minimal_metadata(
    dataset = TRUE, length = TRUE, label = TRUE, type = TRUE, format = TRUE,
    order = TRUE
  )

  # Divert all messages to tempfile, instead of printing them
  #  note: be aware as this should only be used in tests that don't track
  #        messages
  withr::local_message_sink(tempfile())

  xportr_metadata(adsl, metadata) %>%
    attr("_xportr.df_arg_") %>%
    expect_equal("adsl")
  xportr_label(adsl, metadata) %>%
    attr("_xportr.df_arg_") %>%
    expect_equal("adsl")
  xportr_length(adsl, metadata) %>%
    attr("_xportr.df_arg_") %>%
    expect_equal("adsl")
  xportr_order(adsl, metadata) %>%
    attr("_xportr.df_arg_") %>%
    expect_equal("adsl")
  xportr_format(adsl, metadata) %>%
    attr("_xportr.df_arg_") %>%
    expect_equal("adsl")
  xportr_type(adsl, metadata) %>%
    attr("_xportr.df_arg_") %>%
    expect_equal("adsl")
})


test_that("xportr_*: Domain is kept in between calls", {
  # Divert all messages to tempfile, instead of printing them
  #  note: be aware as this should only be used in tests that don't track
  #        messages
  withr::local_message_sink(tempfile())

  adsl <- minimal_table(30)

  metadata <- minimal_metadata(
    dataset = TRUE, length = TRUE, label = TRUE, type = TRUE, format = TRUE,
    order = TRUE
  )

  df2 <- adsl %>%
    xportr_type(metadata)

  df3 <- df2 %>%
    xportr_label(metadata) %>%
    xportr_length(metadata) %>%
    xportr_order(metadata) %>%
    xportr_format(metadata)

  expect_equal(attr(df3, "_xportr.df_arg_"), "adsl")

  df4 <- adsl %>%
    xportr_type(metadata)

  df5 <- df4 %>%
    xportr_label(metadata) %>%
    xportr_length(metadata) %>%
    xportr_order(metadata) %>%
    xportr_format(metadata)

  expect_equal(attr(df5, "_xportr.df_arg_"), "adsl")
})

test_that("xportr_*: Can use magrittr pipe and aquire domain from call", {
  # Divert all messages to tempfile, instead of printing them
  #  note: be aware as this should only be used in tests that don't track
  #        messages
  withr::local_message_sink(tempfile())

  adsl <- minimal_table(30)

  metadata <- minimal_metadata(
    dataset = TRUE, length = TRUE, label = TRUE, type = TRUE, format = TRUE,
    order = TRUE
  )

  non_standard_name <- adsl
  result <- non_standard_name %>%
    xportr_type(metadata) %>%
    xportr_label(metadata) %>%
    xportr_length(metadata) %>%
    xportr_order(metadata) %>%
    xportr_format(metadata) %>%
    xportr_df_label(metadata)

  expect_equal(attr(result, "_xportr.df_arg_"), "non_standard_name")

  # Different sequence call by moving first and last around
  result2 <- non_standard_name %>%
    xportr_label(metadata) %>%
    xportr_length(metadata) %>%
    xportr_order(metadata) %>%
    xportr_df_label(metadata) %>%
    xportr_type(metadata) %>%
    xportr_format(metadata)

  expect_equal(attr(result2, "_xportr.df_arg_"), "non_standard_name")
})

test_that("xportr_*: Can use magrittr pipe and aquire domain from call (metadata)", {
  # Divert all messages to tempfile, instead of printing them
  #  note: be aware as this should only be used in tests that don't track
  #        messages
  withr::local_message_sink(tempfile())

  adsl <- minimal_table(30)

  metadata <- minimal_metadata(
    dataset = TRUE, length = TRUE, label = TRUE, type = TRUE, format = TRUE,
    order = TRUE
  )

  non_standard_name <- adsl
  result <- non_standard_name %>%
    xportr_metadata(metadata) %>%
    xportr_type() %>%
    xportr_label() %>%
    xportr_length() %>%
    xportr_order() %>%
    xportr_format() %>%
    xportr_df_label()

  expect_equal(attr(result, "_xportr.df_arg_"), "non_standard_name")

  # Different sequence call by moving first and last around
  result2 <- non_standard_name %>%
    xportr_metadata(metadata) %>%
    xportr_label() %>%
    xportr_length() %>%
    xportr_order() %>%
    xportr_df_label() %>%
    xportr_type() %>%
    xportr_format()

  expect_equal(attr(result2, "_xportr.df_arg_"), "non_standard_name")
})

test_that("xportr_*: Can use R native pipe (R>4.1) and aquire domain from call", {
  skip_if(
    compareVersion(glue("{R.version$major}.{R.version$minor}"), "4.1.0") < 0,
    "R Version doesn't support native pipe (<4.1)"
  )

  # Divert all messages to tempfile, instead of printing them
  #  note: be aware as this should only be used in tests that don't track
  #        messages
  withr::local_message_sink(tempfile())

  adsl <- minimal_table(30)

  metadata <- minimal_metadata(
    dataset = TRUE, length = TRUE, label = TRUE, type = TRUE, format = TRUE,
    order = TRUE
  )

  non_standard_name_native <- adsl
  result <- non_standard_name_native |>
    xportr_type(metadata) |>
    xportr_label(metadata) |>
    xportr_length(metadata) |>
    xportr_order(metadata) |>
    xportr_format(metadata) |>
    xportr_df_label(metadata)

  expect_equal(attr(result, "_xportr.df_arg_"), "non_standard_name_native")

  # Different sequence call by moving first and last around
  result2 <- non_standard_name_native |>
    xportr_label(metadata) |>
    xportr_length(metadata) |>
    xportr_order(metadata) |>
    xportr_df_label(metadata) |>
    xportr_type(metadata) |>
    xportr_format(metadata)

  expect_equal(attr(result2, "_xportr.df_arg_"), "non_standard_name_native")
})

test_that("xportr_*: Can use R native pipe (R>4.1) and aquire domain from call (metadata)", {
  skip_if(
    compareVersion(glue("{R.version$major}.{R.version$minor}"), "4.1.0") < 0,
    "R Version doesn't support native pipe (<4.1)"
  )

  # Divert all messages to tempfile, instead of printing them
  #  note: be aware as this should only be used in tests that don't track
  #        messages
  withr::local_message_sink(tempfile())

  adsl <- minimal_table(30)

  metadata <- minimal_metadata(
    dataset = TRUE, length = TRUE, label = TRUE, type = TRUE, format = TRUE,
    order = TRUE
  )

  non_standard_name_native <- adsl
  result <- non_standard_name_native |>
    xportr_metadata(metadata) |>
    xportr_type() |>
    xportr_label() |>
    xportr_length() |>
    xportr_order() |>
    xportr_format() |>
    xportr_df_label()

  expect_equal(attr(result, "_xportr.df_arg_"), "non_standard_name_native")

  # Different sequence call by moving first and last around
  result2 <- non_standard_name_native |>
    xportr_metadata(metadata) |>
    xportr_label() |>
    xportr_length() |>
    xportr_order() |>
    xportr_df_label() |>
    xportr_type() |>
    xportr_format()

  expect_equal(attr(result2, "_xportr.df_arg_"), "non_standard_name_native")
})

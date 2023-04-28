test_that("xportr_*: Domain is kept in between calls", {
  withr::local_options(list(xportr.type_verbose = "message"))

  adsl <- dplyr::tibble(
    USUBJID = c(1001, 1002, 1003),
    SITEID = c(001, 002, 003),
    ADATE = readr::parse_date(c("2023-04-11", "2023-04-12", "2023-04-13")),
    AGE = c(63, 35, 27),
    SEX = c("M", "F", "M")
  )

  metadata <- dplyr::tibble(
    dataset = "adsl",
    variable = c("USUBJID", "SITEID", "ADATE", "AGE", "SEX"),
    label = c("Unique Subject Identifier", "Study Site Identifier", "Study Dates", "Age", "Sex"),
    type = c("character", "character", "character", "numeric", "character"),
    length = c(10, 10, 10, 8, 10),
    format = c(NA, NA, "DATE9.", NA, NA),
    order = c(1, 2, 3, 4, 5)
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

test_that("xportr_*: Can use magrittr to pipe", {
  withr::local_options(list(xportr.type_verbose = "message"))

  adsl <- dplyr::tibble(
    USUBJID = c(1001, 1002, 1003),
    SITEID = c(001, 002, 003),
    ADATE = readr::parse_date(c("2023-04-11", "2023-04-12", "2023-04-13")),
    AGE = c(63, 35, 27),
    SEX = c("M", "F", "M")
  )

  metadata <- dplyr::tibble(
    dataset = "adsl",
    variable = c("USUBJID", "SITEID", "ADATE", "AGE", "SEX"),
    label = c("Unique Subject Identifier", "Study Site Identifier", "Study Dates", "Age", "Sex"),
    type = c("character", "character", "character", "numeric", "character"),
    length = c(10, 10, 10, 8, 10),
    format = c(NA, NA, "DATE9.", NA, NA),
    order = c(1, 2, 3, 4, 5)
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

  # xportr_type in a different order
  result2 <- non_standard_name %>%
    xportr_label(metadata) %>%
    xportr_length(metadata) %>%
    xportr_order(metadata) %>%
    xportr_df_label(metadata) %>%
    xportr_type(metadata) %>%
    xportr_format(metadata)

  expect_equal(attr(result2, "_xportr.df_arg_"), "non_standard_name")
})

test_that("xportr_*: Can use R native pipe (R>4.1)", {
  skip_if(
    compareVersion(glue("{R.version$major}.{R.version$minor}"), "4.1.0") < 0,
    "R Version doesn't support native pipe (<4.1)"
  )

  withr::local_options(list(xportr.type_verbose = "message"))

  adsl <- dplyr::tibble(
    USUBJID = c(1001, 1002, 1003),
    SITEID = c(001, 002, 003),
    ADATE = readr::parse_date(c("2023-04-11", "2023-04-12", "2023-04-13")),
    AGE = c(63, 35, 27),
    SEX = c("M", "F", "M")
  )

  metadata <- dplyr::tibble(
    dataset = "adsl",
    variable = c("USUBJID", "SITEID", "ADATE", "AGE", "SEX"),
    label = c("Unique Subject Identifier", "Study Site Identifier", "Study Dates", "Age", "Sex"),
    type = c("character", "character", "character", "numeric", "character"),
    length = c(10, 10, 10, 8, 10),
    format = c(NA, NA, "DATE9.", NA, NA),
    order = c(1, 2, 3, 4, 5)
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

  # xportr_type in a different order
  result2 <- non_standard_name |>
    xportr_label(metadata) |>
    xportr_length(metadata) |>
    xportr_order(metadata) |>
    xportr_df_label(metadata) |>
    xportr_type(metadata) |>
    xportr_format(metadata)

  expect_equal(attr(result2, "_xportr.df_arg_"), "non_standard_name")
})

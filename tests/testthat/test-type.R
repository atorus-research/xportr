meta_example <- data.frame(
  dataset = "df",
  variable = c("Subj", "Param", "Val", "NotUsed"),
  type = c("numeric", "character", "numeric", "character")
)

df <- data.frame(
  Subj = as.character(123, 456, 789),
  Different = c("a", "b", "c"),
  Val = c("1", "2", "3"),
  Param = c("param1", "param2", "param3")
)

test_that("variable types are coerced as expected and can raise messages", {
  expect_message(
    df2 <- xportr_type(df, meta_example),
    "-- Variable type mismatches found. --"
  )

  expect_equal(purrr::map_chr(df2, class), c(
    Subj = "numeric", Different = "character",
    Val = "numeric", Param = "character"
  ))

  expect_error(xportr_type(df, meta_example, verbose = "stop"))

  expect_warning(df3 <- xportr_type(df, meta_example, verbose = "warn"))
  expect_equal(purrr::map_chr(df3, class), c(
    Subj = "numeric", Different = "character",
    Val = "numeric", Param = "character"
  ))

  expect_message(df4 <- xportr_type(df, meta_example, verbose = "message"))
  expect_equal(purrr::map_chr(df4, class), c(
    Subj = "numeric", Different = "character",
    Val = "numeric", Param = "character"
  ))
})

test_that("xportr_metadata: var types coerced as expected and raise messages", {
  expect_message(
    df2 <- xportr_metadata(df, meta_example) %>% xportr_type(),
    "-- Variable type mismatches found. --"
  )

  expect_equal(purrr::map_chr(df2, class), c(
    Subj = "numeric", Different = "character",
    Val = "numeric", Param = "character"
  ))

  expect_error(
    xportr_metadata(df, meta_example) %>% xportr_type(verbose = "stop")
  )

  expect_warning(
    df3 <- xportr_metadata(df, meta_example) %>% xportr_type(verbose = "warn")
  )
  expect_equal(purrr::map_chr(df3, class), c(
    Subj = "numeric", Different = "character",
    Val = "numeric", Param = "character"
  ))

  expect_message(
    df4 <- xportr_metadata(df, meta_example) %>%
      xportr_type(verbose = "message")
  )
  expect_equal(purrr::map_chr(df4, class), c(
    Subj = "numeric", Different = "character",
    Val = "numeric", Param = "character"
  ))
})

test_that("xportr_type(): retains column attributes, besides class", {
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
    format = c(NA, NA, "DATE9.", NA, NA)
  )

  df_type_label <- adsl %>%
    xportr_type(metadata) %>%
    xportr_label(metadata) %>%
    xportr_length(metadata) %>%
    xportr_format(metadata)

  df_label_type <- adsl %>%
    xportr_label(metadata) %>%
    xportr_length(metadata) %>%
    xportr_format(metadata) %>%
    xportr_type(metadata)

  expect_equal(df_type_label, df_label_type)
})

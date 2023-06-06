meta_example <- data.frame(
  dataset = "df",
  variable = c("Subj", "Param", "Val", "NotUsed"),
  type = c("numeric", "character", "numeric", "character"),
  format = NA
)

df <- data.frame(
  Subj = as.character(123, 456, 789),
  Different = c("a", "b", "c"),
  Val = c("1", "2", "3"),
  Param = c("param1", "param2", "param3")
)

test_that("xportr_type: NAs are handled as expected", {
  # Namely that "" isn't converted to NA or vice versa
  # Numeric columns will become NA but that is the nature of as.numeric
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

  df2 <- xportr_type(df, meta_example)
  expect_equal(
    df2,
    structure(
      list(
        Subj = c(123, 456, 789, NA, NA, NA),
        Different = c("a", "b", "c", "", NA, NA),
        Val = c(1, 2, 3, NA, NA, NA),
        Param = c("param1", "param2", "param3", "", NA, NA)
      ),
      row.names = c(NA, -6L),
      `_xportr.df_arg_` = "df",
      class = "data.frame"
    )
  )
})


test_that("xportr_type: Variable types are coerced as expected and can raise messages", {
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

test_that("xportr_metadata: Var types coerced as expected and raise messages", {
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

test_that("xportr_type: Variables retain column attributes, besides class", {
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


test_that("xportr_type: expect error when domain is not a character", {
  df <- data.frame(x = 1, y = 2)
  df_meta <- data.frame(
    variable = c("x", "y"),
    type = "text",
    label = c("X Label", "Y Label"),
    length = c(1, 2),
    common = NA_character_,
    format = c("date9.", "datetime20.")
  )
  expect_error(xportr_type(df, df_meta, domain = 1))
  expect_error(xportr_type(df, df_meta, domain = NA))
})

test_that("xportr_type: works fine from metacore spec", {
  df <- data.frame(x = 1, y = 2)
  metacore_meta <- suppressWarnings(
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
  )
  processed_df <- xportr_type(df, metacore_meta)
  expect_equal(processed_df$x, "1")
})

test_that("xportr_type: error when metadata is not set", {
  expect_error(
    xportr_type(df),
    regexp = "Metadata must be set with `metacore` or `xportr_metadata\\(\\)`"
  )
})

test_that("xportr_type: date variables are not converted to numeric", {
  df <- data.frame(RFICDT = as.Date('2017-03-30'), RFICDTM = as.POSIXct('2017-03-30'))
  metacore_meta <- suppressWarnings(
    metacore::metacore(
      var_spec = data.frame(
        variable = c("RFICDT", "RFICDTM"),
        type = "integer",
        label = c("RFICDT Label", "RFICDTM Label"),
        length = c(1, 2),
        common = NA_character_,
        format = c("date9.", "datetime20.")
      )
    )
  )
  processed_df <- xportr_type(df, metacore_meta)
  expect_equal(lapply(df, class), lapply(processed_df, class))
  expect_equal(df$RFICDT, processed_df$RFICDT)
  expect_equal(df$RFICDTM, processed_df$RFICDTM)
})

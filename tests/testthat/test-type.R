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

  df2 <- suppressMessages(
    xportr_type(df, meta_example)
  )

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
  # Remove empty lines in cli theme
  local_cli_theme()

  (df2 <- xportr_type(df, meta_example)) %>%
    expect_message("Variable type mismatches found.") %>%
    expect_message("[0-9+] variables coerced")

  expect_equal(purrr::map_chr(df2, class), c(
    Subj = "numeric", Different = "character",
    Val = "numeric", Param = "character"
  ))

  expect_error(xportr_type(df, meta_example, verbose = "stop"))

  (df3 <- suppressMessages(xportr_type(df, meta_example, verbose = "warn"))) %>%
    expect_warning()

  expect_equal(purrr::map_chr(df3, class), c(
    Subj = "numeric", Different = "character",
    Val = "numeric", Param = "character"
  ))

  # Ignore other messages
  suppressMessages(
    (df4 <- xportr_type(df, meta_example, verbose = "message")) %>%
      expect_message("Variable type\\(s\\) in dataframe don't match metadata")
  )

  expect_equal(purrr::map_chr(df4, class), c(
    Subj = "numeric", Different = "character",
    Val = "numeric", Param = "character"
  ))
})

test_that("xportr_metadata: Var types coerced as expected and raise messages", {
  # Remove empty lines in cli theme
  local_cli_theme()

  (
    df2 <- xportr_metadata(df, meta_example) %>%
      xportr_type()
  ) %>%
    expect_message("Variable type mismatches found.") %>%
    expect_message("[0-9+] variables coerced")

  expect_equal(purrr::map_chr(df2, class), c(
    Subj = "numeric", Different = "character",
    Val = "numeric", Param = "character"
  ))

  suppressMessages(
    xportr_metadata(df, meta_example) %>% xportr_type(verbose = "stop")
  ) %>%
    expect_error()

  suppressMessages(
    df3 <- xportr_metadata(df, meta_example) %>% xportr_type(verbose = "warn")
  ) %>%
    expect_warning()

  expect_equal(purrr::map_chr(df3, class), c(
    Subj = "numeric", Different = "character",
    Val = "numeric", Param = "character"
  ))

  suppressMessages({
    (
      df4 <- xportr_metadata(df, meta_example)
        %>% xportr_type(verbose = "message")
    ) %>%
      expect_message("Variable type\\(s\\) in dataframe don't match metadata: `Subj` and `Val`")
  })

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

  # Remove empty lines in cli theme
  local_cli_theme()

  # Divert all messages to tempfile, instead of printing them
  #  note: be aware as this should only be used in tests that don't track
  #        messages
  withr::local_message_sink(tempfile())

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
  processed_df <- suppressMessages(
    xportr_type(df, metacore_meta)
  )
  expect_equal(processed_df$x, "1")
})

test_that("xportr_type: error when metadata is not set", {
  expect_error(
    xportr_type(df),
    regexp = "Metadata must be set with `metadata` or `xportr_metadata\\(\\)`"
  )
})

test_that("xportr_type: date variables are not converted to numeric", {
  df <- data.frame(RFICDT = as.Date("2017-03-30"), RFICDTM = as.POSIXct("2017-03-30"))
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
  expect_message(
    {
      processed_df <- xportr_type(df, metacore_meta)
    },
    NA
  )
  expect_equal(lapply(df, class), lapply(processed_df, class))
  expect_equal(df$RFICDT, processed_df$RFICDT)
  expect_equal(df$RFICDTM, processed_df$RFICDTM)

  xportr_write(processed_df, file.path(tempdir(), "dfdates.xpt"))
  df_xpt <- read_xpt(file.path(tempdir(), "dfdates.xpt"))

  expect_equal(lapply(df, class), lapply(df_xpt, class))
  expect_equal(df$RFICDT, df_xpt$RFICDT, ignore_attr = TRUE)
  expect_equal(as.character(df$RFICDTM), as.character(df_xpt$RFICDTM), ignore_attr = TRUE)

  metadata <- data.frame(
    dataset = c("adsl", "adsl", "adsl", "adsl"),
    variable = c("USUBJID", "DMDTC", "RFICDT", "RFICDTM"),
    type = c("text", "date", "integer", "integer"),
    format = c(NA, NA, "date9.", "datetime15.")
  )

  adsl_original <- data.frame(
    USUBJID = c("test1", "test2"),
    DMDTC = c("2017-03-30", "2017-01-08"),
    RFICDT = c("2017-03-30", "2017-01-08"),
    RFICDTM = c("2017-03-30", "2017-01-08")
  )


  adsl_original$RFICDT <- as.Date(adsl_original$RFICDT)
  adsl_original$RFICDTM <- as.POSIXct(adsl_original$RFICDTM)

  expect_message(adsl_xpt2 <- adsl_original %>%
    xportr_type(metadata), NA)

  attr(adsl_original, "_xportr.df_arg_") <- "adsl_original"

  expect_equal(adsl_original, adsl_xpt2)
})

test_that("xportr_type: Gets warning when metadata has multiple rows with same variable", {
  # This test uses the (2) functions below to reduce code duplication
  # All `expect_*` are being called inside the functions
  #
  # Checks that message appears when xportr.domain_name is invalid
  multiple_vars_in_spec_helper(xportr_type)
  # Checks that message doesn't appear when xportr.domain_name is valid
  multiple_vars_in_spec_helper2(xportr_type)
})

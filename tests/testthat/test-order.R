suppressWarnings({
  library(haven)
  library(readxl)
})

test_that("xportr_order: Variable are ordered correctly for data.frame spec", {
  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])
  df_meta <- data.frame(
    dataset = "df",
    variable = letters[1:4],
    order = 1:4
  )

  ordered_df <- xportr_order(df, df_meta)

  expect_equal(names(ordered_df), df_meta$variable)
})

test_that("xportr_order: Variable are ordered correctly when data is piped", {
  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])
  df_meta <- data.frame(
    dataset = "df",
    variable = letters[1:4],
    order = 1:4
  )

  ordered_df <- df %>%
    xportr_order(df_meta) %>%
    xportr_order(df_meta)

  expect_equal(names(ordered_df), df_meta$variable)
})

test_that("xportr_order: Variable are ordered correctly for custom domain", {
  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])
  df_meta <- data.frame(
    dataset = "DOMAIN",
    variable = letters[1:4],
    order = 1:4
  )

  ordered_df <- xportr_order(df, df_meta, domain = "DOMAIN")

  expect_equal(names(ordered_df), df_meta$variable)
})

test_that("xportr_order: Variable are ordered correctly for metacore spec", {
  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])
  ordered_columns <- letters[1:4]
  metacore_meta <- suppressWarnings(
    metacore::metacore(
      ds_vars = data.frame(
        dataset = "df",
        variable = ordered_columns,
        keep = TRUE,
        key_seq = NA,
        order = 1:4,
        core = NA_character_,
        supp_flag = NA
      )
    )
  )

  ordered_df <- xportr_order(df, metacore_meta)

  expect_equal(names(ordered_df), ordered_columns)
})

test_that("xportr_order: Variable are ordered when custom domain_name is passed", {
  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])
  df_meta <- data.frame(
    custom_domain = "df",
    variable = letters[1:4],
    order = 1:4
  )

  ordered_df <- xportr_order(df, df_meta, domain = "df")

  expect_equal(names(ordered_df), df_meta$variable)
})

test_that("xportr_order: Expect error if domain is not a character", {
  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])
  df_meta <- data.frame(
    custom_domain = "df",
    variable = letters[1:4],
    order = 1:4
  )

  expect_error(xportr_order(df, df_meta, domain = NA, verbose = "none"))
  expect_error(xportr_order(df, df_meta, domain = 1, verbose = "none"))
})

test_that("xportr_order: Variable ordering messaging is correct", {
  output_file <- tempfile()

  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])
  df2 <- data.frame(a = "a", z = "z")
  df_meta <- data.frame(
    dataset = "df",
    variable = letters[1:4],
    order = 1:4
  )

  capture.output(xportr_order(df, df_meta, verbose = "message"), file = output_file, type = "message")

  expect_equal(
    readLines(output_file),
    c(
      "-- All variables in specification file are in dataset --",
      "",
      "-- 4 reordered in dataset --",
      "",
      "Variable reordered in `.df`: `a`, `b`, `c`, and `d`"
      )
    )

  capture.output(xportr_order(df2, df_meta, verbose = "message"), file = output_file, type = "message")

  expect_equal(
    readLines(output_file),
    c(
      "-- 2 variables not in spec and moved to end --",
      "",
      "Variable moved to end in `.df`: `a` and `z`",
      "-- All variables in dataset are ordered --",
      ""
    )
  )

})

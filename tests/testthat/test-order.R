test_that("xportr_order: Variable are ordered correctly for data.frame spec", {
  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])
  df_meta <- data.frame(
    dataset = "df",
    variable = letters[1:4],
    order = 1:4
  )

  ordered_df <- suppressMessages(xportr_order(df, df_meta))

  expect_equal(names(ordered_df), df_meta$variable)
})

test_that("xportr_order: Variable are ordered correctly when data is piped", {
  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])
  df_meta <- data.frame(
    dataset = "df",
    variable = letters[1:4],
    order = 1:4
  )

  ordered_df <- suppressMessages(
    df %>%
      xportr_order(df_meta) %>%
      xportr_order(df_meta)
  )

  expect_equal(names(ordered_df), df_meta$variable)
})

test_that("xportr_order: Variable are ordered correctly for custom domain", {
  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])
  df_meta <- data.frame(
    dataset = "DOMAIN",
    variable = letters[1:4],
    order = 1:4
  )

  ordered_df <- suppressMessages(
    xportr_order(df, df_meta, domain = "DOMAIN")
  )

  expect_equal(names(ordered_df), df_meta$variable)
})

test_that("xportr_order: Variable are ordered correctly for metacore spec", {
  skip_if_not_installed("metacore")

  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])
  ordered_columns <- letters[1:4]
  metacore_meta <- suppressMessages(suppressWarnings(
    metacore::metacore(
      ds_vars = data.frame(
        dataset = "df",
        variable = ordered_columns,
        keep = TRUE,
        key_seq = NA,
        order = 1:4,
        core = NA_character_,
        supp_flag = NA
      ),
      # ds_spec required to avoid empty line output
      ds_spec = dplyr::tibble(
        dataset = "df"
      )
    )
  ))

  ordered_df <- suppressMessages(
    xportr_order(df, metacore_meta)
  )

  expect_equal(names(ordered_df), ordered_columns)
})

test_that("xportr_order: Variable are ordered when custom domain_name is passed", {
  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])
  df_meta <- data.frame(
    custom_domain = "df",
    variable = letters[1:4],
    order = 1:4
  )

  ordered_df <- suppressMessages(
    xportr_order(df, df_meta, domain = "df")
  )

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

test_that("xportr_order: error when metadata is not set", {
  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])

  expect_error(
    xportr_order(df),
    regexp = "Metadata must be set with `metadata` or `xportr_metadata\\(\\)`"
  )
})

test_that("xportr_order: Variable ordering messaging is correct", {
  skip_if_not_installed("haven")
  skip_if_not_installed("readxl")

  require(haven, quietly = TRUE)
  require(readxl, quietly = TRUE)

  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])
  df2 <- data.frame(a = "a", z = "z")
  df_meta <- data.frame(
    dataset = "df",
    variable = letters[1:4],
    order = 1:4
  )

  # Remove empty lines in cli theme
  local_cli_theme()

  xportr_order(df, df_meta, verbose = "message") %>%
    expect_message("All variables in specification file are in dataset") %>%
    expect_condition("4 reordered in dataset") %>%
    expect_message("Variable reordered in `.df`: `a`, `b`, `c`, and `d`")

  xportr_order(df2, df_meta, verbose = "message") %>%
    expect_message("2 variables not in spec and moved to end") %>%
    expect_message("Variable moved to end in `.df`: `a` and `z`") %>%
    expect_message("All variables in dataset are ordered")
})

test_that("xportr_order: Metadata order columns are coersed to numeric", {
  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])
  df_meta <- data.frame(
    dataset = "df",
    variable = letters[1:4],
    order = c("1", "2", "11", "90")
  )

  ordered_df <- suppressMessages(
    xportr_order(df, df_meta)
  )

  expect_equal(names(ordered_df), df_meta$variable)
})

test_that("xportr_order: Gets warning when metadata has multiple rows with same variable", {
  # This test uses the (2) functions below to reduce code duplication
  # All `expect_*` are being called inside the functions
  #
  # Checks that message appears when xportr.domain_name is invalid
  multiple_vars_in_spec_helper(xportr_order) %>%
    # expect_message() are being caught to provide clean test without output
    expect_message("All variables in specification file are in dataset") %>%
    expect_message("All variables in dataset are ordered")

  # Checks that message doesn't appear when xportr.domain_name is valid
  multiple_vars_in_spec_helper2(xportr_order) %>%
    # expect_message() are being caught to provide clean test without output
    expect_message("All variables in specification file are in dataset") %>%
    expect_message("All variables in dataset are ordered")
})

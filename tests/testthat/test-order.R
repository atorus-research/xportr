# xportr_order ----
## Test 1: xportr_order: Variable are ordered correctly for data.frame spec ----
test_that("order Test 1: Variable are ordered correctly for data.frame spec", {
  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])
  df_meta <- data.frame(
    dataset = "df",
    variable = letters[1:4],
    order = 1:4
  )

  ordered_df <- suppressMessages(xportr_order(df, df_meta, domain = "df"))

  expect_equal(names(ordered_df), df_meta$variable)
})

## Test 2: xportr_order: Variable are ordered correctly when data is piped ----
test_that("order Test 2: Variable are ordered correctly when data is piped", {
  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])
  df_meta <- data.frame(
    dataset = "df",
    variable = letters[1:4],
    order = 1:4
  )

  ordered_df <- suppressMessages(
    df %>%
      xportr_metadata(domain = "df") %>%
      xportr_order(df_meta) %>%
      xportr_order(df_meta)
  )

  expect_equal(names(ordered_df), df_meta$variable)
})

## Test 3: xportr_order: Variable are ordered correctly for custom domain ----
test_that("order Test 3: Variable are ordered correctly for custom domain", {
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

## Test 4: xportr_order: Variable are ordered correctly for metacore spec ----
test_that("order Test 4: Variable are ordered correctly for metacore spec", {
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
    xportr_order(df, metacore_meta, domain = "df")
  )

  expect_equal(names(ordered_df), ordered_columns)
})

## Test 5: xportr_order: Variable are ordered when custom domain_name is passed ----
test_that("order Test 5: Variable are ordered when custom domain_name is passed", {
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

## Test 6: xportr_order: Expect error if domain is not a character ----
test_that("order Test 6: Expect error if domain is not a character", {
  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])
  df_meta <- data.frame(
    custom_domain = "df",
    variable = letters[1:4],
    order = 1:4
  )

  expect_error(xportr_order(df, df_meta, domain = NA, verbose = "none"))
  expect_error(xportr_order(df, df_meta, domain = 1, verbose = "none"))
})

## Test 7: xportr_order: error when metadata is not set ----
test_that("order Test 7: error when metadata is not set", {
  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])

  expect_error(
    xportr_order(df),
    regexp = "Must be of type 'data.frame', 'Metacore' or set via 'xportr_metadata\\(\\)'"
  )
})

## Test 8: xportr_order: Variable ordering messaging is correct ----
test_that("order Test 8: Variable ordering messaging is correct", {
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
  suppressMessages(
    xportr_order(df, df_meta, verbose = "message", domain = "df") %>%
      expect_message("All variables in dataset are found in `metadata`") %>%
      expect_condition("4 reordered in dataset") %>%
      expect_message("Variable reordered in `.df`: `a`, `b`, `c`, and `d`")
  )

  suppressMessages(xportr_order(df2, df_meta, verbose = "message", domain = "df2") %>%
    expect_message("2 variables not in spec and moved to end") %>% # nolint
    expect_message("Variable moved to end in `.df`: `a` and `z`") %>%
    expect_message("All variables in dataset are ordered"))
})

## Test 9: xportr_order: Metadata order columns are coersed to numeric ----
test_that("order Test 9: Metadata order columns are coersed to numeric", {
  df <- data.frame(c = 1:5, a = "a", d = 5:1, b = LETTERS[1:5])
  df_meta <- data.frame(
    dataset = "df",
    variable = letters[1:4],
    order = c("1", "2", "11", "90")
  )

  ordered_df <- suppressMessages(
    xportr_order(df, df_meta, domain = "df")
  )

  expect_equal(names(ordered_df), df_meta$variable)
})

## Test 10: xportr_order: Gets warning when metadata has multiple rows with same variable ----
test_that("order Test 10: Gets warning when metadata has multiple rows with same variable", {
  # This test uses the (2) functions below to reduce code duplication
  # All `expect_*` are being called inside the functions
  #
  # Checks that message appears when xportr.domain_name is invalid
  suppressMessages(multiple_vars_in_spec_helper(xportr_order) %>%
    # expect_message() are being caught to provide clean test without output      #nolint
    expect_message("All variables in dataset are found in `metadata`") %>%
    expect_message("All variables in dataset are ordered"))

  # Checks that message doesn't appear when xportr.domain_name is valid
  suppressMessages(multiple_vars_in_spec_helper2(xportr_order) %>%
    # expect_message() are being caught to provide clean test without output     #nolint
    expect_message("All variables in dataset are found in `metadata`") %>%
    expect_message("All variables in dataset are ordered"))
})


## Test 11: xportr_order: Works as expected with only one domain in metadata ----
test_that("order Test 11: Works as expected with only one domain in metadata", {
  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    BRTHDT = c(1, 1, 2)
  )

  metadata <- data.frame(
    dataset = c("adsl", "adsl"),
    variable = c("USUBJID", "BRTHDT"),
    order = c(1, 2)
  )

  expect_equal(xportr_order(adsl, metadata), adsl)
})

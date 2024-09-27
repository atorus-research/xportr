## Test 1: xportr_df_label: deprecated metacore gives an error ----
test_that("deprecation Test 1: xportr_df_label: deprecated metacore gives an error", {
  local_options(lifecycle_verbosity = "quiet")
  df <- data.frame(x = "a", y = "b")
  df_meta <- data.frame(dataset = "df", label = "Label")

  expect_error(xportr_df_label(df, metacore = df_meta))
})

## Test 2: xportr_format: deprecated metacore gives an error ----
test_that("deprecation Test 2: xportr_format: deprecated metacore gives an error", {
  local_options(lifecycle_verbosity = "quiet")
  df <- data.frame(x = 1, y = 2)
  df_meta <- data.frame(
    dataset = "df",
    variable = "x",
    format = "date9."
  )

  expect_error(xportr_format(df, metacore = df_meta))
})

## Test 3: xportr_label: using the deprecated metacore argument gives an error ----
test_that(
  "deprecation Test 3: xportr_label: using the deprecated metacore argument gives an error",
  {
    local_options(lifecycle_verbosity = "quiet")

    df <- data.frame(x = "a", y = "b")
    df_meta <-
      data.frame(
        dataset = "df",
        variable = "x",
        label = "foo"
      )

    expect_error(xportr_label(df, metacore = df_meta))
  }
)

## Test 4: xportr_length: using the deprecated metacore argument gives an error ----
test_that(
  "deprecation Test 4: xportr_length: using the deprecated metacore argument gives an error",
  {
    local_options(lifecycle_verbosity = "quiet")
    df <- data.frame(x = "a", y = "b")
    df_meta <- data.frame(
      dataset = "df",
      variable = c("x", "y"),
      type = c("text", "text"),
      length = c(1, 2)
    )

    expect_error(xportr_length(df, metacore = df_meta))
  }
)

## Test 5: xportr_order: using the deprecated metacore argument gives an error ----
test_that(
  "deprecation Test 5: xportr_order: using the deprecated metacore argument gives an error",
  {
    local_options(lifecycle_verbosity = "quiet")

    df <- data.frame(
      c = 1:5,
      a = "a",
      d = 5:1,
      b = LETTERS[1:5]
    )
    df_meta <- data.frame(
      dataset = "DOMAIN",
      variable = letters[1:4],
      order = 1:4
    )

    expect_error(xportr_order(df, metacore = df_meta, domain = "DOMAIN"))
  }
)

## Test 6: xportr_type: using the deprecated metacore argument gives an error ----
test_that(
  "deprecation Test 6: xportr_type: using the deprecated metacore argument gives an error",
  {
    local_options(lifecycle_verbosity = "quiet")
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

    expect_error(xportr_type(df, metacore = df_meta))
  }
)

## Test 7: xportr_split: using deprecated function gives a warning----
test_that(
  "deprecation xportr_split: using deprecated function gives a warning",
  {
    adlb <- data.frame(
      USUBJID = c(1001, 1002, 1003),
      LBCAT = c("HEMATOLOGY", "HEMATOLOGY", "CHEMISTRY")
    )

    expect_warning(adlb <- xportr_split(adlb, "LBCAT"))
  }
)

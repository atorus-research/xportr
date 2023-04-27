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

test_that("Domain not in character format", {
  ADAE <- read_sas(system.file("extdata", "adae.sas7bdat", package = "xportr"))
  met <- read_excel(system.file("specs", "ADaM_spec.xlsx", package = "xportr"), 3)

  expect_error(
    withr::with_options(
      list(xportr.order_name = "Order", xportr.variable_name = "Variable"),
      {
        ADAE_xportr <- xportr_order(ADAE, metacore = met, domain = ADAE, verbose = "none")
      }
    )
  )
})

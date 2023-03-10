
meta_example <- data.frame(
  dataset = "df",
  variable = c("Subj", "Param", "Val", "NotUsed"),
  type = c("numeric", "character", "numeric", "character")
)

df <- data.frame(
 Subj = as.character(c(123, 456, 789)),
 Different = c("a", "b", "c"),
 Val = c("1", "2", "3"),
 Param = c("param1", "param2", "param3")
)

test_that("NAs are handled as expected", {
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
    type = c("numeric", "character", "numeric", "character")
  )
  
  df2 <- xportr_type(df, meta_example)
  expect_equal(
    df2,
    structure(
      list(
        Subj = c(123, 456, 789, NA, NA, NA),
        Different = c("a", "b", "c", "", NA, NA),
        Val = c(1, 2, 3, NA, NA, NA),
        Param = c("param1",  "param2", "param3", "", NA, NA)
      ),
      row.names = c(NA, -6L), 
      `_xportr.df_arg_` = "df",
      class = "data.frame"
    )
  )
})

test_that("variable types are coerced as expected and can raise messages", {

  expect_message(df2 <- xportr_type(df, meta_example),
                 "-- Variable type mismatches found. --")
  
  expect_equal(purrr::map_chr(df2, class), c(Subj = "numeric", Different = "character",
                                      Val = "numeric", Param = "character"))

  expect_error(xportr_type(df, meta_example, verbose = "stop"))

  expect_warning(df3 <- xportr_type(df, meta_example, verbose = "warn"))
  expect_equal(purrr::map_chr(df3, class), c(Subj = "numeric", Different = "character",
                                      Val = "numeric", Param = "character"))

  expect_message(df4 <- xportr_type(df, meta_example, verbose = "message"))
  expect_equal(purrr::map_chr(df4, class), c(Subj = "numeric", Different = "character",
                                      Val = "numeric", Param = "character"))})


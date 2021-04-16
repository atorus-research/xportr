
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


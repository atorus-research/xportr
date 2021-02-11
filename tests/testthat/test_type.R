
meta_example <- data.frame(
  Variable = c("Subj", "Param", "Val", "NotUsed"),
  Type = c("numeric", "character", "numeric", "character")
)

df <- data.frame(
 Subj = as.character(123, 456, 789),
 Different = c("a", "b", "c"),
 Val = c("1", "2", "3"),
 Param = c("param1", "param2", "param3")
)

test_that("variable types are coerced as expected and can raise messages", {

  expect_silent(df2 <- xpt_coerce_variable_type(df, meta_example))
  expect_equal(map_chr(df2, class), c(Subj = "numeric", Different = "character",
                                      Val = "numeric", Param = "character"))

  expect_error(xpt_coerce_variable_type(df, meta_example, verbose = "stop"),
               "Your data types do not match")

  expect_warning(df3 <- xpt_coerce_variable_type(df, meta_example, verbose = "warn"),
                 "Your data types do not match")
  expect_equal(map_chr(df3, class), c(Subj = "numeric", Different = "character",
                                      Val = "numeric", Param = "character"))

  expect_message(df4 <- xpt_coerce_variable_type(df, meta_example, verbose = "message"),
                 "Your data types do not match")
  expect_equal(map_chr(df4, class), c(Subj = "numeric", Different = "character",
                                      Val = "numeric", Param = "character"))})

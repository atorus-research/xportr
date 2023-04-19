test_that("Type parameter will create correct messages", {

  xportr_logger("A message", type = "none") %>%
    expect_silent()

  xportr_logger("A message", type = "message") %>%
    expect_message("A message")

  xportr_logger("A message", type = "warn") %>%
    expect_warning("A message")

  xportr_logger("A message", type = "stop") %>%
    expect_error("A message")

  # Supports additional parameters to rlang::stop
  xportr_logger("A message", type = "stop", footer = "A footer") %>%
    expect_error("A message", class = "rlang_error")
})

test_that("Logs names of missed variables", {

  # Prevent CLI messages
  withr::local_options(list(cli.default_handler = function(...) { }))

  expect_message(
    label_log(c("var1", "var2", "var3"), "message"),
    "Problem with `var1`.*`var2`.*`var3`"
  )
})

test_that("Variables reorderd message", {
  moved_vars <- c("var1", "var2", "var3")
  message_regexp <- "Variable reordered in.+`var1`.+`var2`.+`var3`$"

  expect_message(
    var_ord_msg(moved_vars, "message"),
    regexp = message_regexp
  )

  expect_error(
    var_ord_msg(moved_vars, "stop"),
    regexp = message_regexp
  )
})

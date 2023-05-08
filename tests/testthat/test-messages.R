#' Test `R/messages.R` functions

test_that("xportr_logger: Type parameter will create correct message type", {
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

test_that("length_log: Missing lengths messages are shown", {
  # Remove empty lines in cli theme
  withr::local_options(list(cli.user_theme = cli_theme_tests))
  app <- cli::start_app(output = "message", .auto_close = FALSE)
  withr::defer(cli::stop_app(app))

  length_log(c("var1", "var2", "var3"), "message") %>%
    expect_message("Variable lengths missing from metadata.") %>%
    expect_message("lengths resolved") %>%
    expect_message("Problem with `var1`.*`var2`.*`var3`")
})

test_that("length_log: Missing variables messages are shown", {
  # Remove empty lines in cli theme
  withr::local_options(list(cli.user_theme = cli_theme_tests))
  app <- cli::start_app(output = "message", .auto_close = FALSE)
  withr::defer(cli::stop_app(app))

  label_log(c("var1", "var2", "var3"), "message") %>%
    # cli messages
    expect_message("Variable labels missing from metadata.") %>%
    expect_message("labels skipped") %>%
    # xportr_logger messages
    expect_message("Problem with `var1`.*`var2`.*`var3`")
})

test_that("var_ord_msg: Reordered variables messages are shown", {
  # Remove empty lines in cli theme
  withr::local_options(list(cli.user_theme = cli_theme_tests))
  app <- cli::start_app(output = "message", .auto_close = FALSE)
  withr::defer(cli::stop_app(app))

  moved_vars <- c("var1", "var2", "var3")
  message_regexp <- "Variable reordered in.+`var1`.+`var2`.+`var3`$"

  var_ord_msg(moved_vars, "message") %>%
    expect_message("variables not in spec and moved to end") %>%
    expect_message(message_regexp)

  var_ord_msg(c(), "message") %>%
    expect_message("All variables in specification file are in dataset")
})

test_that("var_names_log: Renamed variables messages are shown", {
  # Remove empty lines in cli theme
  withr::local_options(list(cli.user_theme = cli_theme_tests))
  app <- cli::start_app(output = "message", .auto_close = FALSE)
  withr::defer(cli::stop_app(app))

  tidy_names_df <- data.frame(
    original_varname = c("var1", "var2", "var3", "var4", "VAR5", "VAR6"),
    renamed_var = c("VAR1", "VAR2", "VAR3", "VAR4", "VAR5", "VAR6"),
    col_pos = seq(1, 6),
    renamed_msg = glue("renamed message {seq(1, 6)}"),
    renamed_n = 0
  )

  tidy_names_df %>%
    mutate(
      renamed_n = c(
        2,
        sample(c(0, 1, 2), size = NROW(.data$renamed_n) - 1, replace = TRUE)
      )
    ) %>%
    var_names_log("message") %>%
    expect_message(
      ".*[0-9]+ of [0-9]+ \\([0-9]+(\\.[0-9]+)%\\) variables were renamed.*"
    ) %>%
    expect_message("Var . : '.*' was renamed to '.*'") %>%
    expect_message("Var . : '.*' was renamed to '.*'") %>%
    expect_message("Var . : '.*' was renamed to '.*'") %>%
    expect_message("Var . : '.*' was renamed to '.*'") %>%
    expect_message("Duplicate renamed term\\(s\\) were created")
})
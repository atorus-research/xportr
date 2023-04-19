test_cli_theme <- list(
  cli.user_theme = list(h2 = list(`margin-top` = 0, `margin-bottom` = 0))
)

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

test_that("Length", {
  # Prevent CLI messages
  withr::local_options(cli.user_theme = test_cli_theme)

  # Catch CLI messages
  length_log(c("var1", "var2", "var3"), "none") %>%
    expect_message("Variable lengths missing from metadata.") %>%
    expect_message("3 lengths resolved")

  # Prevent CLI messages
  withr::local_options(list(cli.default_handler = function(...) { }))

  length_log(c("var1", "var2", "var3"), "message") %>%
    expect_message("Problem with `var1`.*`var2`.*`var3`")
})

test_that("Logs names of missed variables", {
  # Prevent CLI messages
  withr::local_options(cli.user_theme = test_cli_theme)

  # Catch CLI messages
  label_log(c("var1", "var2", "var3"), "none") %>%
    expect_message("Variable labels missing from metadata.") %>%
    expect_message("3 labels skipped")

  # Prevent CLI messages
  withr::local_options(list(cli.default_handler = function(...) { }))

  label_log(c("var1", "var2", "var3"), "message") %>%
    expect_message("Problem with `var1`.*`var2`.*`var3`")
})

test_that("Variables reorderd message", {
  # Prevent CLI messages
  withr::local_options(cli.user_theme = test_cli_theme)

  # Catch CLI messages
  moved_vars <- c("var1", "var2", "var3")
  message_regexp <- "Variable reordered in.+`var1`.+`var2`.+`var3`$"

  var_ord_msg(moved_vars, "none") %>%
    expect_message("3 variables not in spec and moved to end")

  var_ord_msg(c(), "none") %>%
    expect_message("All variables in specification file are in dataset")

  # Prevent CLI messages
  withr::local_options(list(cli.default_handler = function(...) { }))

  var_ord_msg(moved_vars, "message") %>%
    expect_message(message_regexp)

  var_ord_msg(moved_vars, "stop") %>%
    expect_error(message_regexp)
})

test_that("Tidy names rename messages", {
  # Prevent CLI messages
  withr::local_options(cli.user_theme = test_cli_theme)

  tidy_names_df <- dplyr::tibble(
    original_varname = c("var1", "var2", "var3", "var4", "VAR5", "VAR6"),
    renamed_var = c("VAR1", "VAR2", "VAR3", "VAR4", "VAR5", "VAR6"),
    col_pos = seq(1, 6),
    renamed_msg = glue::glue("renamed message {seq(1, 6)}"),
    renamed_n = 0
  )

  tidy_names_df %>%
    dplyr::mutate(
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


#' Test `R/messages.R` functions

# xportr_logger ----
## Test 1: xportr_logger: Type parameter will create correct message type ----
test_that("messages Test 1: Type parameter will create correct message type", {
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

# length_log ----
## Test 2: length_log: Missing lengths messages are shown ----
test_that("messages Test 2: Missing lengths messages are shown", {
  # Remove empty lines in cli theme
  local_cli_theme()

  length_log(c("var1", "var2", "var3"), c("var4"), "message") %>%
    expect_message("Variable lengths missing from metadata.") %>%
    expect_message("lengths resolved `var1`.*`var2`.*`var3`.*`var4`") %>%
    expect_message("Problem with `var1`.*`var2`.*`var3`")
})

## Test 3: length_log: Missing variables messages are shown ----
test_that("messages Test 3: Missing variables messages are shown", {
  # Remove empty lines in cli theme
  local_cli_theme()

  label_log(c("var1", "var2", "var3"), "message") %>%
    # cli messages
    expect_message("Variable labels missing from metadata.") %>%
    expect_message("labels skipped") %>%
    # xportr_logger messages
    expect_message("Problem with `var1`.*`var2`.*`var3`")
})

# var_names_log ----
## Test 4: var_names_log: Renamed variables messages are shown ----
test_that("messages Test 4: Renamed variables messages are shown", {
  # Remove empty lines in cli theme
  local_cli_theme()

  tidy_names_df <- data.frame(
    original_varname = c("var1", "var2", "var3", "var4", "VAR5", "VAR6"),
    renamed_var = c("VAR1", "VAR2", "VAR3", "VAR4", "VAR5", "VAR6"),
    col_pos = seq(1, 6),
    renamed_msg = glue("renamed message {seq(1, 6)}"),
    renamed_n = 0
  )

  tidy_names_df %>%
    mutate(renamed_n = c(
      2,
      sample(
        c(0, 1, 2),
        size = NROW(.data$renamed_n) - 1,
        replace = TRUE
      )
    )) %>%
    var_names_log("message") %>%
    expect_message(".*[0-9]+ of [0-9]+ \\([0-9]+(\\.[0-9]+)%\\) variables were renamed.*") %>%
    expect_message("Var . : '.*' was renamed to '.*'") %>%
    expect_message("Var . : '.*' was renamed to '.*'") %>%
    expect_message("Var . : '.*' was renamed to '.*'") %>%
    expect_message("Var . : '.*' was renamed to '.*'") %>%
    expect_message("Duplicate renamed term\\(s\\) were created")
})

# no_domain_log ----
## Test 5: no_domain_log: No domain messages are shown ----
test_that("messages Test 5: No domain messages are shown", {
  # Remove empty lines in cli theme
  local_cli_theme()

  log_no_domain("adsl", "domains", "message") %>%
    expect_message("Domain not found in metadata.") %>%
    expect_message("Domain 'adsl' not found in metadata 'domains' column.")

  adsl <- data.frame(
    USUBJID = c(1001, 1002, 1003),
    BRTHDT = c(1, 1, 2)
  )

  metadata <- data.frame(
    dataset = c("adsl", "adsl"),
    variable = c("USUBJID", "BRTHDT"),
    order = c(1, 2)
  )

  xportr_order(adsl, metadata, "wrong_adsl", verbose = "message") %>%
    expect_message("Domain not found in metadata.") %>%
    expect_message("Domain 'wrong_adsl' not found in metadata 'dataset' column.") %>%
    expect_message("2 variables not in spec and moved to end") %>%
    expect_message("Variable moved to end in `.df`: `USUBJID` and `BRTHDT`") %>%
    expect_message("All variables in dataset are ordered")
})

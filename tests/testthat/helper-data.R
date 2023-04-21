# Re-usable data for sample ADSL table
minimal_table <- function(n_rows = 3, cols = c("x", "y")) {
  data.frame(
    x = sample(1000 + seq(n_rows * 100), size = n_rows),
    y = sample(c(0, 1, 2), size = n_rows, replace = TRUE),
    z = 3,
    a = 4,
    b = sample(
      c("Recusandae", "vero", "nihil", "velit", "omnis"),
      size = n_rows,
      replace = TRUE
    ),
    c = sample(
      Sys.time() - 3600 * c(1, 10, 100, 1000),
      size = n_rows,
      replace = TRUE
    ),
    d = sample(Sys.Date() + c(1, -1, 10, -10), size = n_rows, replace = TRUE)
  ) %>%
    select(cols)
}

# Re-usable data from sample metadata for ADSL
minimal_metadata <- function(
  dataset = FALSE,
  length = FALSE,
  label = FALSE,
  type = FALSE,
  format = FALSE,
  common = FALSE,
  var_names = NULL
) {
  cols_logical <- c(dataset, TRUE, label, length, type, format, common)
  cols <- c(
    "dataset", "variable", "label", "length", "type", "format", "common"
  )[cols_logical]

  metadata <- tribble(
    ~dataset, ~variable,        ~label, ~length,       ~type,       ~format, ~common,
      "adsl",       "x",       "Lorem",       8,   "numeric",            NA,      NA,
      "adsl",       "y",       "Ipsum",     200,   "numeric",            NA,      NA,
      "adsl",       "z",       "Dolor",       8,   "numeric",            NA,      NA,
      "adsl",       "a",         "Sit",       8,   "numeric",            NA,      NA,
      "adsl",       "b",        "Amet",     200, "character",            NA,      NA,
      "adsl",       "c", "Consectetur",     200, "character", "datetime20.",      NA,
      "adsl",       "d",  "Adipiscing",     200,      "date",      "date9.",      NA
  )

  if (!is.null(var_names)) {
    metadata <- metadata %>%
      filter(.data$variable %in% var_names)
  }

  metadata %>% select(cols)
}

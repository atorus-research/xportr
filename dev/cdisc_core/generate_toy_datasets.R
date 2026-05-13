# Generate toy ADaM xpt datasets using xportr for CDISC CORE rule validation.
#
# Produces an ADSL.xpt built from xportr's bundled example data, fully processed
# through the standard xportr pipeline (type, length, order, format, label,
# df_label, write). Output lands in ./xpt_output for downstream validation.

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(xportr)
})

out_dir <- "xpt_output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

data("adsl_xportr", package = "xportr")

var_spec <- read_xlsx(
  system.file("specs", "ADaM_spec.xlsx", package = "xportr"),
  sheet = "Variables"
) %>%
  rename(type = "Data Type") %>%
  rename_with(tolower)

dataset_spec <- read_xlsx(
  system.file("specs", "ADaM_spec.xlsx", package = "xportr"),
  sheet = "Datasets"
) %>%
  rename_with(tolower) %>%
  rename(label = "description")

adsl_xportr %>%
  xportr_type(var_spec, domain = "ADSL", verbose = "message") %>%
  xportr_length(var_spec, domain = "ADSL", verbose = "message") %>%
  xportr_order(var_spec, domain = "ADSL", verbose = "message") %>%
  xportr_format(var_spec, domain = "ADSL") %>%
  xportr_label(var_spec, domain = "ADSL", verbose = "message") %>%
  xportr_df_label(dataset_spec, domain = "ADSL") %>%
  xportr_write(
    path = file.path(out_dir, "adsl.xpt"),
    domain = "ADSL",
    strict_checks = FALSE
  )

cat("\nGenerated files in", out_dir, ":\n")
print(list.files(out_dir, full.names = TRUE))

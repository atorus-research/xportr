# Generate toy SDTM and ADaM xpt datasets using xportr for CDISC CORE validation.
#
# Writes:
#   xpt_output/dm.xpt    - minimal SDTM DM (Demographics) domain (5 subjects)
#   xpt_output/adsl.xpt  - full ADaM ADSL from xportr's bundled example data
#
# CORE v0.15 bundles ~430 sdtmig 3-4 rules but zero adamig rules, so DM is the
# dataset that will actually exercise rule coverage.

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(xportr)
})

out_dir <- "xpt_output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# --- SDTM DM (Demographics) ---------------------------------------------------
dm <- tibble::tibble(
  STUDYID = "XPORTR-DEMO-01",
  DOMAIN = "DM",
  USUBJID = sprintf("XPORTR-DEMO-01-%03d", 1:5),
  SUBJID = sprintf("%03d", 1:5),
  RFSTDTC = c("2024-01-15", "2024-02-01", "2024-02-10", "2024-03-05", "2024-03-12"),
  RFENDTC = c("2024-07-15", "2024-08-01", "2024-08-10", "2024-09-05", "2024-09-12"),
  RFXSTDTC = c("2024-01-15", "2024-02-01", "2024-02-10", "2024-03-05", "2024-03-12"),
  RFXENDTC = c("2024-07-15", "2024-08-01", "2024-08-10", "2024-09-05", "2024-09-12"),
  SITEID = c("001", "001", "002", "002", "003"),
  AGE = c(45L, 52L, 38L, 61L, 47L),
  AGEU = "YEARS",
  SEX = c("M", "F", "F", "M", "F"),
  RACE = c("WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN", "WHITE", "WHITE"),
  ETHNIC = c(
    "NOT HISPANIC OR LATINO", "NOT HISPANIC OR LATINO",
    "NOT HISPANIC OR LATINO", "HISPANIC OR LATINO",
    "NOT HISPANIC OR LATINO"
  ),
  ARM = c("DRUG A", "DRUG A", "PLACEBO", "DRUG A", "PLACEBO"),
  ARMCD = c("A", "A", "P", "A", "P"),
  ACTARM = c("DRUG A", "DRUG A", "PLACEBO", "DRUG A", "PLACEBO"),
  ACTARMCD = c("A", "A", "P", "A", "P"),
  COUNTRY = c("USA", "USA", "USA", "USA", "USA")
)

xportr_write(
  dm,
  path = file.path(out_dir, "dm.xpt"),
  domain = "DM",
  metadata = data.frame(
    dataset = "dm",
    label = "Demographics"
  ),
  strict_checks = FALSE
)

# --- ADaM ADSL ----------------------------------------------------------------
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

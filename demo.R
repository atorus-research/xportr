# DEMO
devtools::load_all()
library(dplyr)
library(purrr)

adsl <- haven::read_sas("inst/extdata/adsl.sas7bdat")
var_spec <- readxl::read_xlsx("inst/specs/ADaM_spec.xlsx", sheet = "Variables") %>%
  rename(type = "Data Type") %>%
  set_names(tolower)
data_spec <- readxl::read_xlsx("inst/specs/ADaM_spec.xlsx", sheet = "Datasets") %>%
  set_names(tolower) %>%
  rename(label = "description")
  
adsl <- adsl %>%
  xportr_type(var_spec, "ADSL", "message") %>%
  xportr_length(var_spec, "ADSL", "message") %>%
  xportr_label(var_spec, "ADSL", "message") %>%
  xportr_df_label(data_spec, "ADSL") %>%
  # xportr_core(var_spec, "ADSL") %>% Moving to CDISCUtils
  # xportr_seq(var_spec, "ADSL") %>% Assumption about data
  xportr_write("adsl.xpt")


### Still todo:
# 1) Improve error messages when metadata is in unexpected form
# 2) Document verbose settings
# 3) Complete unit testing

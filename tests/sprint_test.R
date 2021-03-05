adae <- haven::read_sas("inst/extdata/adae.sas7bdat")
ds_spec <- readxl::read_excel("inst/specs/ADaM_spec.xlsx", "Datasets")
spec <- readxl::read_excel("inst/specs/ADaM_spec.xlsx", "Variables")

adae %>%
  xportr_label(spec) %>%
  xportr_dflabel(ds_spec) %>%
  xportr_format(spec) %>%
  xportr_core(spec) %>%
  xportr_type(spec) %>%
  xportr_seq(spec) %>%
  xportr_write(spec, "xportr_output.xpt")

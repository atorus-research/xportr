library(dplyr)
library(haven)
library(testthat)

#context("xportr_seq correctly order dataset according to spec")

test_that("Variables are correctly grabbed from the Spec file", {
  
  spec_manual <- c("STUDYID", "USUBJID","SUBJID","SITEID","AGE","SEX","RACE","ACOUNTRY","FASFL","ITTFL")
  
  spec_xportr <- get_spec_col_names("ADAE", tab_model="ADAM", 
                                    path_to_spec = "~/xportr/inst/specs", vendor = "GSK")[1:10]
  
  expect_equal(spec_manual, spec_xportr)
})


test_that("Variable are ordered correctly", {
  
  ADAE <- read_sas("~/xptr/inst/extdata/adae.sas7bdat")
  
  ADAE_xportr <- xportr_ord("ADAE", ADAE, tab_model = "ADAM", 
                            path_to_spec = "~/xportr/inst/specs", 
                            vendor = "GSK", verbose = FALSE,
                            msg_var_order = TRUE)
  
  vars_in_spec_ds <- c("STUDYID","SITEID", "USUBJID","SUBJID", "TRT01A", "TRT01AN",
                       "AETERM", "AEDECOD","AESOC","AGE","SEX","RACE","SAFFL","ASTDT",
                       "ASTTM","AENDT","ADURC","AEACN","AESER","AEOUT", "AEREL","ATOXGR", 
                       "ATOXGRN","AFTRTSTC")
  
  # Grabs all variables from Spec file and orders accordingly
  seq_vars <- ADAE %>% 
    select(all_of(vars_in_spec_ds)) 
  
  # Variables not in Spec file - will be moved to the end
  drop_vars <- ADAE %>% 
    select(!all_of(vars_in_spec_ds))
  
  ADAE_manual <- bind_cols(seq_vars, drop_vars)
  
  expect_equal(ADAE_xportr, ADAE_manual)
})


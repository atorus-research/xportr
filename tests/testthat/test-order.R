# library(dplyr)
suppressWarnings({
  library(haven)
  library(readxl)
})

# 
# #context("xportr_seq correctly order dataset according to spec")
#

test_that("Variable are ordered correctly", {

  ADAE <- read_sas(system.file("extdata", "adae.sas7bdat", package = "xportr"))
  met <- read_excel(system.file("specs", "ADaM_spec.xlsx", package = "xportr"), 3)

  withr::with_options(
    list(xportr.order_name = "Order", xportr.variable_name = "Variable"), {
      ADAE_xportr <- xportr_order(ADAE, metacore = met, "ADAE", verbose = "none")
    }
  )
  
  after_names <- c("STUDYID", "USUBJID", "AEDECOD", "AESOC", "AETERM", "AESER", 
                   "ASTDT", "AENDT", "ATOXGR", "TRT01A", "TRT01AN", "SAFFL", "SUBJID", 
                   "WEIGHTBL", "SEX", "AGE", "AGEU", "RACE", "SITEID", "RACEN", 
                   "ASTTM", "ADURC", "AEACN", "AEOUT", "AEREL", "ATOXGRN", "AFTRTSTC", 
                   "AEWDFL")

  expect_equal(names(ADAE_xportr), after_names)
})

test_that("Domain not in character format", {
  
  ADAE <- read_sas(system.file("extdata", "adae.sas7bdat", package = "xportr"))
  met <- read_excel(system.file("specs", "ADaM_spec.xlsx", package = "xportr"), 3)
  
  expect_error(
    withr::with_options(
    list(xportr.order_name = "Order", xportr.variable_name = "Variable"), {
      ADAE_xportr <- xportr_order(ADAE, metacore = met, domain = ADAE, verbose = "none")
    }
  ))
  
   
})


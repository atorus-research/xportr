library(haven)
library(stringr)
library(dplyr)
library(xfun)
library(purrr)
library(tidyr)


context("Variable, Labels, ASCII and ? Tests")

test_that("Variable with lower case are found", {
  
  adsl <- read_sas("~/xptr/inst/extdata/adsl.sas7bdat")
  adsl_lower <- adsl %>% rename_with(tolower) %>% select(studyid, sex, randdt)
  adsl_removed <- adsl %>% select(-STUDYID, -SEX, -RANDDT)
  adsl_cmb <- bind_cols(adsl_lower, adsl_removed)
  
  
  test_exp <- tibble( Variable  = 
                     c("studyid",
                       "sex",
                       "randdt"
                     ),
                   
                     Flag  =
                     
                     c(FALSE,
                       FALSE,
                       FALSE
                     ))
  
  expect_identical(check_var_case(adsl_cmb), test_exp)
  
})

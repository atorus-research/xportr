library(haven)
library(stringr)
library(dplyr)
library(xfun)
library(purrr)
library(tidyr)
library(tibble)

context("Variable, Labels, ASCII and ? Tests")


test_that("Variables have length <= 8", {
      
  adsl <- read_sas("~/xptr/inst/extdata/adsl.sas7bdat")
  adsl_renamed <- adsl %>% rename("STUDYIDSTUDYID" = STUDYID)    
  
  test_var_len_exp <- tibble( value  = 
                             c("STUDYIDSTUDYID"
                             ),
                          var_length  =
                             c(14L
                             ))
  expect_identical(xpt_check_var_length(adsl_renamed), test_var_len_exp)
  
  
          })

test_that("Variable with lower case are found", {
  
  adsl <- read_sas("~/xptr/inst/extdata/adsl.sas7bdat")
  adsl_lower <- adsl %>% rename_with(tolower) %>% select(studyid, sex, randdt)
  adsl_removed <- adsl %>% select(-STUDYID, -SEX, -RANDDT)
  adsl_cmb <- bind_cols(adsl_lower, adsl_removed)
  
  test_vars_exp <- tibble( Variable  = 
                     c("studyid",
                       "sex",
                       "randdt"
                     ),
                   
                     Flag  =
                     
                     c(FALSE,
                       FALSE,
                       FALSE
                     ))
  
  expect_identical(xpt_check_var_case(adsl_cmb), test_vars_exp)
  
})

# Need a test for no issues found with variable case


test_that("Found variable labels length that are too long!", {
  
adsl_lbls <- adsl %>% add_labels(
  USUBJID = "Unique Subject ID's",
  AGE = "Age of Subject at Start of Study Age of Subject at Start of Study")

test_lbls_exp <- tibble( name  = 
                           c("AGE"
                           ),
                         value  =
                           c("Age of Subject at Start of Study Age of Subject at Start of Study"
                           ),
                         label_length =
                           c(65L))

expect_identical(xpt_check_label_length(adsl_lbls), test_lbls_exp)

})

# Need a test for no issues found with variable case

test_that("non-scii Characters found in Variable Lables", {
  
test_ascii_data <- tibble( `À` = 
                             c("Subj1",
                               "Subj2",
                               "Subj3"
                             ),
                           `Team Members` =
                             c("Test1",
                               "Test2",
                               "Test3"
                             ))

test_ascii_data_lbls <- teams %>% add_labels(
  `À` = "Unique Subject ÀD's",
  `Team Members` = "ÀTest")

test_ascii_exp <- tibble( name = 
                            c("À",
                              "Team Members"
                            ),
                          value =
                            c("Unique Subject ÀD's",
                              "ÀTest"
                            ),
                          Flag = 
                            c("non-ASCII Found",
                              "non-ASCII Found"))


expect_identical(xpt_check_ascii_lbls(test_ascii_data_lbls), test_ascii_exp)

})

test_that("non-scii Characters found in Variable Names", {
  
  test_ascii_data <- tibble( `À` = 
                               c("Subj1",
                                 "Subj2",
                                 "Subj3"
                               ),
                             `Team Members` =
                               c("Test1",
                                 "Test2",
                                 "Test3"
                               ))
  
  expect_identical(xpt_check_ascii_vars(test_ascii_data), test_ascii_exp)
  
})

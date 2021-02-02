library(testthat)
library(haven)
library(stringr)
library(dplyr)
library(xfun)
library(purrr)
library(tidyr)
library(tibble)
library(stringi)

context("Variable, Labels, ASCII and ? Tests")


test_that("Variables have length <= 8", {
  
  library(haven)
  
  path <- system.file("extdata", "adsl.sas7bdat", package = "xptr")   
  adsl <- haven::read_sas(path)
  adsl_renamed <- adsl %>% rename("STUDYIDSTUDYID" = STUDYID)    
  
  test_var_len_exp <- tibble::tibble( value  = 
                             c("STUDYIDSTUDYID"
                             ),
                          var_length  =
                             c(14L
                             ))
  expect_identical(xpt_check_var_length(adsl_renamed), test_var_len_exp)
  
  
          })

test_that("Variable with lower case are found", {
  
  library(haven)
  
  path <- system.file("extdata", "adsl.sas7bdat", package = "xptr")   
  adsl <- haven::read_sas(path)
  adsl_lower <- adsl %>% rename_with(tolower) %>% select(studyid, sex, randdt)
  adsl_removed <- adsl %>% select(-STUDYID, -SEX, -RANDDT)
  adsl_cmb <- bind_cols(adsl_lower, adsl_removed)
  
  test_vars_exp <- tibble( value  = 
                     c("studyid",
                       "sex",
                       "randdt"
                     ),
                   
                     flag  =
                     
                     c(FALSE,
                       FALSE,
                       FALSE
                     ))
  
  expect_identical(xpt_check_var_case(adsl_cmb), test_vars_exp)
  
})

# Need a test for no issues found with variable case


test_that("Found variable labels length that are too long!", {
  
  library(haven)
  
  path <- system.file("extdata", "adsl.sas7bdat", package = "xptr")   
  adsl <- haven::read_sas(path)
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

test_that("non-ASCII Characters found in Variable Names", {
  
  test_ascii_data <- tibble( dum = 
                               c("Subj1",
                                 "Subj2",
                                 "Subj3"
                               ),
                             `Team Members` =
                               c("Test1",
                                 "Test2",
                                 "Test3"
                               ))
  
  colnames(test_ascii_data) <- c(stri_unescape_unicode('\\u00c0'), "Team Members")
  
  test_ascii_exp <- tibble( value = 
                              c(stri_unescape_unicode('\\u00c0'),
                                "Team Members"
                              ),
                            flag = 
                              c(strtrim("non-ASCII Found", 15),
                                "All ASCII"))
  
  expect_equal(xpt_check_ascii_vars(test_ascii_data), test_ascii_exp)
  
})


test_that("non-ASCII Characters found in Variable Lables", {
  
test_ascii_data <- tibble(dum = 
                             c("Subj1",
                               "Subj2",
                               "Subj3"
                             ),
                           `Team Members` =
                             c("Test1",
                               "Test2",
                               "Test3"
                             ))

test_ascii_data_lbls <- test_ascii_data %>% add_labels(
  dum = paste0("Unique Subject ", stri_unescape_unicode('\\u00c0'), "Ds"),
  `Team Members` = "Test")

colnames(test_ascii_data_lbls) <- c(stri_unescape_unicode('\\u00c0'), "Team Members")



test_ascii_lbl_exp <- tibble( 
                          name = 
                            c(stri_unescape_unicode('\\u00c0')),
                          value =
                            c(paste0("Unique Subject ", stri_unescape_unicode('\\u00c0'), "Ds")),
                          flag = 
                            c(strtrim("non-ASCII Found", 15)))


expect_identical(xpt_check_ascii_lbls(test_ascii_data_lbls), test_ascii_lbl_exp)

})




vars <- c("", "STUDYID", "studyid", "subject id", "1c. ENT", "1b. Eyes",
                "1d. Lungs", "1e. Heart", "year number", "1a. Skin_Desc")
adxx <- data.frame(matrix(0, ncol = 10, nrow = 3))
colnames(adxx) <- vars

my_dictionary <- data.frame(original_varname = "subject id", dict_varname = "subjid")


test_that("Variable names are renamed according to expected default behavior", {

  expect_equal(
    
      colnames(xportr_varnames(adxx)),
               c("V1", "STUDYID", "STUDYID2", "SUBJECTD", "ENT1C", "EYES1B",
                 "LUNGS1D", "HEART1E", "YEARNUMB", "SKNDSC1A")
      )
  
})

test_that("Dicationary lookup works", {
  
  expect_equal(colnames(xportr_varnames(adxx, dict_dat  = my_dictionary)),
               c("V1", "STUDYID", "STUDYID2", "SUBJID", "ENT1C", "EYES1B",
                 "LUNGS1D", "HEART1E", "YEARNUMB", "SKNDSC1A"))
  
})
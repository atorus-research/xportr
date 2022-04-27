suppressWarnings({
  library(haven)
  library(readxl)
})

test_that("Domain not in character format", {
  
  ADAE <- read_sas(system.file("extdata", "adae.sas7bdat", package = "xportr"))
  met <- read_excel(system.file("specs", "ADaM_spec.xlsx", package = "xportr"), 3)
  
  expect_error(
       xportr_length(ADAE, metacore = met, domain = ADAE, verbose = "none")
    )
  
})

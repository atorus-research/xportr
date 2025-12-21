## Test 1: xportr_split: using deprecated function gives a warning----
test_that(
  "deprecation xportr_split: using deprecated function gives a warning",
  {
    adlb <- data.frame(
      USUBJID = c(1001, 1002, 1003),
      LBCAT = c("HEMATOLOGY", "HEMATOLOGY", "CHEMISTRY")
    )

    expect_warning(adlb <- xportr_split(adlb, "LBCAT"))
  }
)

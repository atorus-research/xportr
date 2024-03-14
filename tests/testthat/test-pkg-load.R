## Test 1: .onLoad: Unset options get initialised on package load with defaults ----
test_that("pkg-load Test 1: .onLoad: Unset options get initialised on package load with defaults", {
  skip_if(getOption("testthat_interactive"))
  with_options(
    {
      expect_no_error(.onLoad())
      expect_equal(getOption("xportr.df_domain_name"), "dataset")
    },
    xportr.df_domain_name = NULL
  )
})

## Test 2: .onLoad: Initialised options are retained and not overwritten ----
test_that("pkg-load Test 2: .onLoad: Initialised options are retained and not overwritten", {
  skip_if(getOption("testthat_interactive"))
  with_options(
    {
      expect_no_error(.onLoad())
      expect_equal(getOption("xportr.df_domain_name"), "custom_domain")
    },
    xportr.df_domain_name = "custom_domain"
  )
})

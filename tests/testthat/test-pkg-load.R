test_that(".onLoad: Unset options get initialised on package load with defaults", {
  skip_if(getOption("testthat_interactive"))
  withr::with_options(
    list(xportr.df_domain_name = NULL),
    {
      expect_no_error(.onLoad())
      expect_equal(getOption("xportr.df_domain_name"), "dataset")
    }
  )
})

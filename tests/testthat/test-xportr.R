test_that("pipeline results match `xportr()` results", {
  if (require(magrittr, quietly = TRUE)) {
    skip_if_not_installed("withr")
    pipeline_path <- withr::local_file("adslpipe.xpt")
    xportr_path <- withr::local_file("adslxptr.xpt")

    dataset_spec_low <- setNames(dataset_spec, tolower(names(dataset_spec)))
    names(dataset_spec_low)[[2]] <- "label"

    var_spec_low <- setNames(var_spec, tolower(names(var_spec)))
    names(var_spec_low)[[5]] <- "type"

    # Divert all messages to tempfile, instead of printing them
    #  note: be aware as this should only be used in tests that don't track
    #        messages
    withr::local_message_sink(withr::local_tempfile())
    pipeline_df <- adsl %>%
      xportr_metadata(var_spec_low, "ADSL", verbose = "none") %>%
      xportr_type() %>%
      xportr_length() %>%
      xportr_label() %>%
      xportr_order() %>%
      xportr_format() %>%
      xportr_df_label(dataset_spec_low) %>%
      xportr_write(pipeline_path)

    # `xportr()` can be used to apply a whole pipeline at once
    xportr_df <- xportr(
      adsl,
      var_metadata = var_spec_low,
      df_metadata = dataset_spec_low,
      domain = "ADSL",
      verbose = "none",
      path = xportr_path
    )

    expect_identical(pipeline_df, xportr_df)

    expect_identical(
      haven::read_xpt(pipeline_path),
      haven::read_xpt(xportr_path)
    )
  }
})

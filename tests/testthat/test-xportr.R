test_that("pipeline results match `xportr()` results", {

  has_pkgs <- requireNamespace("admiral", quietly = TRUE) &&
    requireNamespace("dplyr", quietly = TRUE) &&
    requireNamespace("readxl", quietly = TRUE) &&
    requireNamespace("rlang", quietly = TRUE)

  if (has_pkgs) {
    adsl <- admiral::admiral_adsl

    spec_path <- system.file(paste0("specs/", "ADaM_admiral_spec.xlsx"),
                             package = "xportr")

    var_spec <- readxl::read_xlsx(spec_path, sheet = "Variables") %>%
      dplyr::rename(type = "Data Type") %>%
      rlang::set_names(tolower)
    dataset_spec <- readxl::read_xlsx(spec_path, sheet = "Datasets") %>%
      dplyr::rename(label = "Description") %>%
      rlang::set_names(tolower)

    test_dir <- tempdir()

    pipeline_path <- file.path(test_dir, "adslpipe.xpt")
    xportr_path <- file.path(test_dir, "adslxptr.xpt")

    pipeline_df <- adsl %>%
      xportr_metadata(var_spec, "ADSL", verbose = "none") %>%
      xportr_type() %>%
      xportr_length() %>%
      xportr_label() %>%
      xportr_order() %>%
      xportr_format() %>%
      xportr_df_label(dataset_spec) %>%
      xportr_write(pipeline_path)

    # `xportr()` can be used to apply a whole pipeline at once
    xportr_df <- xportr(adsl,
      var_metadata = var_spec,
      df_metadata = dataset_spec,
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

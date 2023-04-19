#' Test `xportr_length()` function
#'
#' Tests will check for:
#'  * Errors
#' * Result of call will create `SASlength` attribute (`width` for each
#' variable)
#'
#' Helper functions / data (from ´test/testthat/helper-length.R´):
#'  * \code{minimal_adsl} : minimal data frame with ADSL sample
#'  * \code{minimal_length_metadata} : minimal metadata used in
#'    `xport_length()`. It contains dataset (domain), variable names and length
#'  * \code{expect_attr_width(result, metadata_length)} : support function to
#'    test the data.frame modifications being done in `xportr_length()`

test_that("Valid domain names", {
  adsl <- minimal_adsl
  metadata <- minimal_length_metadata

  # Setup temporary options with active verbose
  withr::local_options(list(xportr.length_verbose = "message"))

  # Test minimal call with valid data and without domain
  xportr_length(adsl, metadata) %>%
    expect_silent() %>%
    expect_attr_width(metadata$length)

  # Test minimal call with valid data with a valid domain
  xportr_length(adsl, metadata, domain = "adsl") %>%
    expect_silent() %>%
    expect_attr_width(metadata$length) %>%
    NROW() %>%
    expect_equal(3)

  # Test minimal call without datasets
  metadata_without_dataset <- metadata %>% select(-"dataset")

  xportr_length(adsl, metadata_without_dataset) %>%
    expect_silent() %>%
    expect_attr_width(metadata_without_dataset$length) %>%
    NROW() %>%
    expect_equal(3)


  # Test minimal call without datasets, but valid domain
  xportr_length(adsl, metadata_without_dataset, domain = "adsl") %>%
    expect_silent() %>%
    expect_attr_width(metadata_without_dataset$length) %>%
    NROW() %>%
    expect_equal(3)
})

test_that("CDISC data frame is being piped after another xportr function", {
  adsl <- minimal_adsl
  metadata <- minimal_length_metadata %>%
    bind_cols(type = c("numeric", "numeric"))

  # Setup temporary options with active verbose
  withr::local_options(list(xportr.length_verbose = "message"))

  adsl %>%
    xportr_type(metadata, domain = "adsl", verbose = "message") %>%
    xportr_length(metadata) %>%
    expect_silent() %>%
    expect_attr_width(metadata$length) %>%
    attr("_xportr.df_arg_" ) %>%
    expect_equal("adsl")
})

test_that("CDISC data frame is being piped", {
  adsl <- minimal_adsl
  metadata <- minimal_length_metadata %>%
    bind_cols(type = c("numeric", "numeric"))

  # Setup temporary options with `verbose = "message"`
  withr::local_options(list(xportr.length_verbose = "message"))

  adsl %>%
    xportr_length(metadata, domain = "adsl") %>%
    expect_attr_width(metadata$length) %>%
    attr("_xportr.df_arg_" ) %>%
    expect_equal("adsl")

  # Prevent CLI messages
  withr::local_options(
    list(cli.default_handler = function(...) { })
  )

  # Test message when dataset is set, but domain used in parameters
  expect_message(
    adsl %>% xportr_length(metadata),
    regexp = "Variable\\(s\\) present in dataframe but doesn't exist in `.*`."
  )

  # Test results with piping
  adsl %>% xportr_length(metadata) %>%
    expect_attr_width(metadata$length) %>%
    attr("_xportr.df_arg_" ) %>%
    expect_equal("...")
})

test_that("Impute character lengths based on class", {
  adsl <- minimal_adsl
  metadata <- minimal_length_metadata

  # Setup temporary options with `verbose = "none"`
  withr::local_options(list(xportr.length_verbose = "none"))
  # Prevent CLI messages
  withr::local_options(list(cli.default_handler = function(...) { }))
  # Define controlled `charater_types` for this test
  withr::local_options(list(xportr.character_types = c("character", "date")))

  # Test length imputation of character and numeric (not valid character type)
  adsl %>%
    mutate(
      USUBJID = as.numeric(.data$USUBJID),
      BRTHDT =  as.character(.data$BRTHDT)
    ) %>%
    xportr_length(metadata) %>%
    expect_attr_width(c(8, 200)) %>%
    attr("_xportr.df_arg_" ) %>%
    expect_equal("...")

  # Test length imputation of two valid character types (both should have
  # `widht = 200``)
  adsl %>%
    mutate(
      USUBJID = as.character(.data$USUBJID),
      BRTHDT =  as.Date(.data$BRTHDT, origin = "1970-01-01")
    ) %>%
    xportr_length(metadata) %>%
    expect_attr_width(c(200, 200)) %>%
    attr("_xportr.df_arg_" ) %>%
    expect_equal("...")
})

test_that("Variables not in metacore", {
  adsl <- minimal_adsl
  metadata <- minimal_length_metadata %>%
    filter(variable != "BRTHDT")

  # Prevent CLI messages
  withr::local_options(list(cli.default_handler = function(...) { }))
  # Setup temporary options with `verbose = "message"`
  withr::local_options(list(xportr.length_verbose = "message"))

  # Test that message is given which indicates that variable is not present
  xportr_length(adsl, metadata) %>%
    expect_message(regexp = "Problem with `BRTHDT`")
})

test_that("Metacore instance can be used", {
  adsl <- minimal_adsl

  # Build a minimal metacore object
  metadata <- suppressMessages(
    suppressWarnings(
      metacore::metacore(
        ds_spec = dplyr::tibble(
          dataset = "ADSL"
        ),
        ds_vars = dplyr::tibble(
          dataset = "ADSL",
          variable = colnames(adsl)
        ),
        var_spec = dplyr::tibble(
          variable = c("USUBJID", "BRTHDT"),
          type = c("integer", "integer"),
          length = c(20, 8),
          label = c("Unique Subject Identifier "),
          format = NA_character_,
          common = NA
        )
      )
    )
  )

  # Test metacore parameter with `metacore` class instead of data.frame
  xportr_length(adsl, metadata, domain = "adsl", verbose = "message") %>%
    expect_silent() %>%
    NROW() %>%
    expect_equal(3) %>%
    expect_attr_width(metadata$length)
})

test_that("Domain not in character format", {
  skip_if_not(
    require(haven, quietly = TRUE) && require(readxl, quietly = TRUE), 
    message = "haven or readxl not installed"
  )

  ADAE <- read_sas(system.file("extdata", "adae.sas7bdat", package = "xportr"))
  met <- read_excel(system.file("specs", "ADaM_spec.xlsx", package = "xportr"), 3)

  expect_error(
    xportr_length(ADAE, metacore = met, domain = ADAE, verbose = "none")
  )
})

test_that("Column length of known/unkown character types is 200/8 ", {
  expect_equal(impute_length(123), 8)
  expect_equal(impute_length(123L), 8)
  expect_equal(impute_length("string"), 200)
  expect_equal(impute_length(Sys.Date()), 200)
  expect_equal(impute_length(Sys.time()), 200)

  withr::local_options(list(xportr.character_types = c("character", "date")))
  expect_equal(impute_length(Sys.time()), 8)

})

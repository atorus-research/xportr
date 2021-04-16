
<!-- README.md is generated from README.Rmd. Please edit that file -->

# xportr <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[<img src="https://img.shields.io/badge/Slack-OSTCR-blue?style=flat&logo=slack">](https://ostinclinicalresearch.slack.com)
[<img src="https://img.shields.io/badge/Slack-RValidationHub-blue?style=flat&logo=slack">](https://RValidationHub.slack.com)
[![R build
status](https://github.com/atorus-research/xportr/workflows/R-CMD-check/badge.svg)](https://github.com/atorus-research/xportr/actions?workflow=R-CMD-check)
[<img src="https://img.shields.io/codecov/c/github/atorus-research/xportr">](https://codecov.io/gh/atorus-research/xportr)
[<img src="https://img.shields.io/badge/License-MIT-blue.svg">](https://github.com/atorus-research/xportr/blob/master/LICENSE)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental-1)
<!-- badges: end -->

The goal of xportr is to make the interface between your

## Installation

You can install the released version of xportr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("xportr")
```

## Example

``` r
adsl <- haven::read_sas("inst/extdata/adsl.sas7bdat")

var_spec <- readxl::read_xlsx("inst/specs/ADaM_spec.xlsx", sheet = "Variables") %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower)
  
data_spec <- readxl::read_xlsx("inst/specs/ADaM_spec.xlsx", sheet = "Datasets") %>%
  rlang::set_names(tolower) %>%
  dplyr::rename(label = "description")
  
adsl %>%
  xportr_type(var_spec, "ADSL", "message") %>%
  xportr_length(var_spec, "ADSL", "message") %>%
  xportr_label(var_spec, "ADSL", "message") %>%
  xportr_df_label(data_spec, "ADSL") %>%
  xportr_write("adsl.xpt")
```

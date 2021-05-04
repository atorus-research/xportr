
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

Welcome to `xportr`\! We have designed `xportr` to help get your xpt
files ready for transport either to a Pinnacle 21-like application or to
a regulatory agency\! This package has the functionality to associate
all metadata information to a local R data frame, perform data set level
validation checks and convert into a SAS transport file (xpt)

As always, we welcome your feedback. If you spot a bug, would like to
see a new feature, or if any documentation is unclear - submit an issue
through GitHub right
[here](https://github.com/atorus-gsk/xportr/issues).

## Installation

### Development version:

``` r
devtools::install_github("https://github.com/atorus-gsk/xportr.git")
```

### CRAN

  - Not yet available on CRAN.

# What is xportr?

`xportr` is designed for clinical programmers to create CDISC compliant
data sets - **ADaM** or **SDTM**. Essentially, this package has two big
components to it - writing XPT files with well-defined metadata and checking
compliance of the data sets. The first set of tools are designed to
allow a clinical programmer to build a CDISC compliant XPT file directly from R.
 The second set of tools are to perform checks on your data
sets before you send them to any validators or data reviewers.

<img src="man/figures/design_flow.png">

# What are the checks?

  - Dataset is in upper case
  - Variable names should contain only uppercase letters, numbers, and
    must start with a letter (≤ 8)
  - Allotted length for each column containing character (text) data
    should be set to the maximum length of the variable used across all
    datasets (≤ 200)
  - Display format support for numeric float and date/time values
  - Variable descriptive labels & dataset labels, ≤ 40 & Presence of
    special characters

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

# Where to go from here?

There’s quite a bit more to learn\! And we’ve prepared a number of other
vignettes to help you get what you need out of `xportr`.

# References

Pinnacle 21

metadata

timber

Atorus

GSK

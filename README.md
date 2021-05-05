
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
files ready for transport either to a clinical data set validator
application or to a regulatory agency This package has the functionality
to associate all metadata information to a local R data frame, perform
data set level validation checks and convert into a [transport v5
file(xpt)](https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.5/movefile/n1xbwdre0giahfn11c99yjkpi2yb.htm).

As always, we welcome your feedback. If you spot a bug, would like to
see a new feature, or if any documentation is unclear - submit an issue
on [xportr’s Github
page](https://github.com/atorus-research/xportr/issues).

## Installation

### Development version:

``` r
devtools::install_github("https://github.com/atorus-research/xportr.git")
```

### CRAN

  - As this is an experimental package and under development we have not
    made it available on CRAN.

# What is xportr?

`xportr` is designed for clinical programmers to create CDISC compliant
xpt files- **ADaM** or **SDTM**. Essentially, this package has two big
components to it - writing xpt files with well-defined metadata and
checking compliance of the data sets. The first set of tools are
designed to allow a clinical programmer to build a CDISC compliant xpt
file directly from R. The second set of tools are to perform checks on
your data sets before you send them off to any validators or data
reviewers.

<br>

<img src="man/figures/design_flow.png">

<br>

# What are the checks?

  - Variable names must start with a letter.
  - Variables names are ≤ 8 characters.
  - Allotted length for each column containing character (text) data
    should be set to the maximum length of the variable used across all
    data sets (≤ 200)
  - Coerces variables to only numeric or character types
  - Display format support for numeric float and date/time values
  - Variable labels are ≤ 200 characters.
  - Data set labels are ≤ 40 characters.
  - Presence of non-ASCII characters in Variable Names, Labels or data
    set labels.

**NOTE:** Each check has associated messages and warning.

## Example

The first example involves an ADSL data set in the `.sas7bdat` format
with associated specification in the `.xlsx` format.

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

The second example involves the use of the objects developed within the
`metacore` package

``` r

# metacore stuff goes here
```

# Where to go from here?

Please check out the 3 articles within the Articles Tab:

1)  How to write out an xpt file with `xportr`
2)  Drilling down with `xportr` checks
3)  Interfacing with the `metacore` package.

# References

This package was a developed jointly by
[GSK](https://us.gsk.com/en-us/home/) and
[Atorus](https://www.atorusresearch.com/).

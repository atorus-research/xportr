
<!-- README.md is generated from README.Rmd. Please edit that file -->

# xportr <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[<img src="https://img.shields.io/badge/Slack-RValidationHub-blue?style=flat&logo=slack">](https://RValidationHub.slack.com)
[![R build
status](https://github.com/atorus-research/xportr/workflows/R-CMD-check/badge.svg)](https://github.com/atorus-research/xportr/actions?workflow=R-CMD-check)
[<img src="https://img.shields.io/codecov/c/github/atorus-research/xportr">](https://app.codecov.io/gh/atorus-research/xportr)
[<img src="https://img.shields.io/badge/License-MIT-blue.svg">](https://github.com/atorus-research/xportr/blob/master/LICENSE)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental-1)
<!-- badges: end -->

Welcome to `xportr`! We have designed `xportr` to help get your xpt
files ready for transport either to a clinical data set validator
application or to a regulatory agency. This package has the
functionality to associate metadata information to a local R data frame,
perform data set level validation checks and convert into a [transport
v5
file(xpt)](https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.5/movefile/n1xbwdre0giahfn11c99yjkpi2yb.htm).

As always, we welcome your feedback. If you spot a bug, would like to
see a new feature, or if any documentation is unclear - submit an issue
on [xportr’s GitHub
page](https://github.com/atorus-research/xportr/issues).

## Installation

### Development version:

``` r
devtools::install_github("https://github.com/atorus-research/xportr.git", ref = "main")
```

### CRAN

-   As this is an experimental package and under development we have not
    made it available on CRAN.

# What is xportr?

<br>

`xportr` is designed for clinical programmers to create CDISC compliant
xpt files- **ADaM** or **SDTM**. Essentially, this package has two big
components to it

1.  Writing xpt files with well-defined metadata
2.  Checking compliance of the data sets.

The first set of tools are designed to allow a clinical programmer to
build a CDISC compliant xpt file directly from R. The second set of
tools are to perform checks on your data sets before you send them off
to any validators or data reviewers.

<br>

<img src="man/figures/design_flow.png">

<br>

# What are the checks?

<br>

-   Variable names must start with a letter (not an underscore), be
    comprised of only uppercase letters (A-Z), numerals (0-9) and be
    free of non-ASCII characters, symbols, and underscores.
-   Allotted length for each column containing character (text) data
    should be set to the maximum length of the variable used across all
    data sets (≤ 200)
-   Coerces variables to only numeric or character types
-   Display format support for numeric float and date/time values
-   Variables names are ≤ 8 characters.
-   Variable labels are ≤ 200 characters.
-   Data set labels are ≤ 40 characters.
-   Presence of non-ASCII characters in Variable Names, Labels or data
    set labels.

**NOTE:** Each check has associated messages and warning.

## Simple Example

**Objective:** Create a fully compliant v5 xpt `ADSL` dataset that was
developed using R.

To do this we will need to do the following:

-   Apply types
-   Apply lengths  
-   Apply variable labels
-   Apply formats
-   Re-order the variables
-   Apply a dataset label
-   Write out a version 5 xpt file

All of which can be done using a well-defined specification file and the
`xportr` package!

First we will start with our `ADSL` dataset created in R. This example
`ADSL` dataset is taken from the
[`{admiral}`](https://pharmaverse.github.io/admiral/index.html) package.
The script that generates this `ADSL` dataset can be created by using
this command `admiral::use_ad_template("adsl")`. This `ADSL` dataset has
306 observations and 48 variables.

``` r
library(dplyr)
library(admiral)
library(xportr)

adsl <- admiral::admiral_adsl
```

We have created a dummy specification file called
`ADaM_admiral_spec.xlsx` found in the `specs` folder of this package.
You can use
`system.file(paste0("specs/", "ADaM_admiral_spec.xlsx"), package = "xportr")`
to access this file.

``` r
spec_path <- system.file(paste0("specs/", "ADaM_admiral_spec.xlsx"), package = "xportr")

var_spec <- readxl::read_xlsx(spec_path, sheet = "Variables") %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower)
```

Each `xportr_` function has been written in a way to take in a part of
the specification file and apply that piece to the dataset.

``` r
adsl %>% 
  xportr_type(var_spec, "ADSL") %>%
  xportr_length(var_spec, "ADSL") %>%
  xportr_label(var_spec, "ADSL") %>%
  xportr_order(var_spec, "ADSL") %>% 
  xportr_format(var_spec, "ADSL") %>% 
  xportr_write("adsl.xpt", label = "Subject-Level Analysis Dataset")
```

That’s it! We now have a xpt file created in R with all appropriate
types, lengths, labels, ordering and formats. Please check out the [Get
Started](https://atorus-research.github.io/xportr/articles/xportr.html)
for more information and detailed walk through of each `xportr_`
function.

We are in talks with other Pharma companies involved with the
[`{pharmaverse}`](https://pharmaverse.org/) to enhance this package to
play well with other downstream and upstream packages.

# References

<br>

This package was developed jointly by
[GSK](https://us.gsk.com/en-us/home/) and
[Atorus](https://www.atorusresearch.com/).

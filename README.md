
<!-- README.md is generated from README.Rmd. Please edit that file -->

# xportr <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[<img src="https://img.shields.io/badge/Slack-RValidationHub-blue?style=flat&logo=slack">](https://RValidationHub.slack.com)
[![R build
status](https://github.com/atorus-research/xportr/workflows/R-CMD-check/badge.svg)](https://github.com/atorus-research/xportr/actions?workflow=R-CMD-check)
[<img src="https://img.shields.io/codecov/c/gh/atorus-research/xportr">](https://app.codecov.io/gh/atorus-research/xportr)
[<img src="https://img.shields.io/badge/License-MIT-blue.svg">](https://github.com/atorus-research/xportr/blob/master/LICENSE)
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

This package is available from CRAN and can be installed by running:

``` r
install.packages("xportr")
```

### Development version:

``` r
install.packages("xportr", repos = c("https://pharmaverse.r-universe.dev", getOption("repos")))
```

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

- Variable names must start with a letter (not an underscore), be
  comprised of only uppercase letters (A-Z), numerals (0-9) and be free
  of non-ASCII characters, symbols, and underscores.
- Allotted length for each column containing character (text) data
  should be set to the maximum length of the variable used across all
  data sets (≤ 200)
- Coerces variables to only numeric or character types
- Display format support for numeric float and date/time values
- Variables names are ≤ 8 characters.
- Variable labels are ≤ 40 characters.
- Data set labels are ≤ 40 characters.
- Presence of non-ASCII characters in Variable Names, Labels or data set
  labels.

**NOTE:** Each check has associated messages and warning.

## Simple Example

**Objective:** Create a fully compliant v5 xpt `ADSL` dataset that was
developed using R.

To do this we will need to do the following:

- Apply types
- Apply lengths  
- Apply variable labels
- Apply formats
- Re-order the variables
- Apply a dataset label
- Write out a version 5 xpt file

All of which can be done using a well-defined specification file and the
`{xportr}` package!

First we will start with our `ADSL` dataset created in R. This example
`ADSL` dataset contains 306 observations and 51 variables.

``` r
library(dplyr)
library(xportr)

data("adsl_xportr")
ADSL <- adsl_xportr
```

We have created a dummy specification file called `ADaM_spec.xlsx` found
in the `specs` folder of this package. You can use
`system.file(file.path("specs/", "ADaM_spec.xlsx"), package = "xportr")`
to access this file.

``` r
spec_path <- system.file(file.path("specs", "ADaM_spec.xlsx"), package = "xportr")

var_spec <- readxl::read_xlsx(spec_path, sheet = "Variables") %>%
  dplyr::rename(type = "Data Type") %>%
  dplyr::rename_with(tolower)
dataset_spec <- readxl::read_xlsx(spec_path, sheet = "Datasets") %>%
  dplyr::rename(label = "Description") %>%
  dplyr::rename_with(tolower)
```

Each `xportr_` function has been written in a way to take in a part of
the specification file and apply that piece to the dataset. Setting
`verbose = "warn"` will send appropriate warning message to the console.
We have suppressed the warning for the sake of brevity.

``` r
ADSL %>%
  xportr_metadata(var_spec, "ADSL") %>%
  xportr_type(verbose = "warn") %>%
  xportr_length(verbose = "warn") %>%
  xportr_label(verbose = "warn") %>%
  xportr_order(verbose = "warn") %>%
  xportr_format() %>%
  xportr_df_label(dataset_spec, "ADSL") %>%
  xportr_write("adsl.xpt")
```

The `xportr_metadata()` function can reduce duplication by setting the
variable specification and domain explicitly at the top of a pipeline.
If you would like to use the `verbose` argument, you will need to set in
each function call.

``` r
ADSL %>%
  xportr_metadata(var_spec, "ADSL", verbose = "warn") %>%
  xportr_type() %>%
  xportr_length() %>%
  xportr_label() %>%
  xportr_order() %>%
  xportr_format() %>%
  xportr_df_label(dataset_spec) %>%
  xportr_write("adsl.xpt")
```

Furthermore, if you’re calling all xportr functions at once with common
metadata and verbosity, you can shorten it by simply using `xportr()`.

``` r
xportr(
  .df = ADSL,
  var_metadata = var_spec,
  df_metadata = dataset_spec,
  domain = "ADSL",
  verbose = "warn",
  "adsl.xpt"
)
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

# Order variables of a dataset according to Spec

The
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
function is used to order the columns of the dataframe. Any variables
that are missing an order value are appended to the end of the dataframe
after all of the variables that have an order.

## Usage

``` r
xportr_order(.df, metadata = NULL, domain = NULL, verbose = NULL)
```

## Arguments

- .df:

  A data frame of CDISC standard.

- metadata:

  A metacore object or a data frame containing variable level metadata.
  See 'Metadata' section for details.

- domain:

  Appropriate CDISC dataset name, e.g. ADAE, DM. Used to subset the
  metadata object.

- verbose:

  The action this function takes when an action is taken on the dataset
  or function validation finds an issue. See 'Messaging' section for
  details. Options are 'stop', 'warn', 'message', and 'none'

## Value

Dataframe that has been re-ordered according to spec

## Messaging

There are three primary messages output by `xportr_order()`.

The first identifies the "moved" variables. These are the variables that
were either not found in the metadata file or had missing order values,
and therefore moved to the end of the dataset. A message will be
generated noting the number, if any, of variables that were moved to the
end of the dataset.

The second message identifies the "reordered" variables. These are the
variables that were in the dataset, but not in the correct order. A
message will be generated noting the number, if any, of variables that
have been reordered.

The third message identifies the "skipped" metadata variables. These are
the metadata variables missing from the dataset and therefore skipped
from processing. A message will be generated noting the number, if any,
of metadata variables that have been skipped.

In all three cases, if the value passed to the `verbose` argument is
`stop`, `warn`, or `message`, a complete list of the affected variables
will be provided.

## Metadata

The argument passed in the 'metadata' argument can either be a metacore
object, or a data.frame containing the data listed below. If metacore is
used, no changes to options are required.

For data.frame 'metadata' arguments three columns must be present:

1.  Domain Name - passed as the 'xportr.domain_name' option. Default:
    "dataset". This is the column subset by the 'domain' argument in the
    function.

2.  Variable Name - passed as the 'xportr.variable_name' option.
    Default: "variable". This is used to match columns in '.df' argument
    and the metadata.

3.  Variable Order - passed as the 'xportr.order_name' option. Default:
    "order". These values used to arrange the order of the variables. If
    the values of order metadata are not numeric, they will be coerced
    to prevent alphabetical sorting of numeric values.

## Examples

``` r
adsl <- data.frame(
  BRTHDT = c(1, 1, 2),
  STUDYID = c("mid987650", "mid987650", "mid987650"),
  TRT01A = c("Active", "Active", "Placebo"),
  USUBJID = c(1001, 1002, 1003)
)

metadata <- data.frame(
  dataset = c("adsl", "adsl", "adsl", "adsl"),
  variable = c("STUDYID", "USUBJID", "TRT01A", "BRTHDT"),
  order = 1:4
)

adsl <- xportr_order(adsl, metadata, domain = "adsl")
#> 
#> ── All variables in dataset are found in `metadata` ──
#> 
#> ── 3 reordered in dataset ──
#> 
```

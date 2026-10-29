# Assign Dataset Label

Assigns dataset label from a dataset level metadata to a given data
frame. This is stored in the 'label' attribute of the dataframe.

## Usage

``` r
xportr_df_label(.df, metadata = NULL, domain = NULL)
```

## Arguments

- .df:

  A data frame of CDISC standard.

- metadata:

  A metacore object or a data frame containing dataset level metadata.
  See 'Metadata' section for details.

- domain:

  Appropriate CDISC dataset name, e.g. ADAE, DM. Used to subset the
  metadata object.

## Value

Data frame with label attributes.

## Metadata

The argument passed in the 'metadata' argument can either be a metacore
object, or a data.frame containing the data listed below. If metacore is
used, no changes to options are required.

For data.frame 'metadata' arguments two columns must be present:

1.  Domain Name - passed as the 'xportr.df_domain_name' option. Default:
    "dataset". This is the column subset by the 'domain' argument in the
    function.

2.  Label Name - passed as the 'xportr.df_label' option. Default:
    "label". Character values to update the 'label' attribute of the
    dataframe This is passed to
    [`haven::write_xpt`](https://haven.tidyverse.org/reference/read_xpt.html)
    to note the label.

## Examples

``` r
adsl <- data.frame(
  USUBJID = c(1001, 1002, 1003),
  SITEID = c(001, 002, 003),
  AGE = c(63, 35, 27),
  SEX = c("M", "F", "M")
)

metadata <- data.frame(
  dataset = c("adsl", "adae"),
  label = c("Subject-Level Analysis", "Adverse Events Analysis")
)

adsl <- xportr_df_label(adsl, metadata, domain = "adsl")
```

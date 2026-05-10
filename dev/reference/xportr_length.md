# Assign SAS Length

Assigns the SAS length to a specified data frame, either from a metadata
object or based on the calculated maximum data length. If a length isn't
present for a variable the length value is set to maximum data length
for character columns, and 8 for non-character columns. This value is
stored in the 'width' attribute of the column.

## Usage

``` r
xportr_length(
  .df,
  metadata = NULL,
  domain = NULL,
  verbose = NULL,
  length_source = c("metadata", "data")
)
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

- length_source:

  Choose the assigned length from either metadata or data.

  If `"metadata"` is specified, the assigned length is from the metadata
  length. If `"data"` is specified, the assigned length is determined by
  the calculated maximum data length.

  *Permitted Values*: `"metadata"`, `"data"`

## Value

Data frame with SAS default length attributes for each variable.

## Messaging

If there are any columns present in the '.df' that are not noted in the
metadata, or any variables in metadata that have missing length values,
their length is set to maximum data length for character columns, and 8
for non-character columns. A message will be generated noting the number
of those variables.

If there are variables in the metadata that don't exist in '.df', they
will be skipped and a message will be generated noting the number of
metadata variables skipped.

In both cases, if the value passed to the 'verbose' argument is 'stop',
'warn', or 'message', a complete list of the affected variables will be
provided.

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

3.  Variable Label - passed as the 'xportr.length' option. Default:
    "length". These numeric values to update the 'width' attribute of
    the column. This is passed to
    [`haven::write_xpt`](https://haven.tidyverse.org/reference/read_xpt.html)
    to note the variable length.

## Examples

``` r
adsl <- data.frame(
  USUBJID = c(1001, 1002, 1003),
  BRTHDT = c(1, 1, 2)
)

metadata <- data.frame(
  dataset = c("adsl", "adsl"),
  variable = c("USUBJID", "BRTHDT"),
  length = c(10, 8)
)

adsl <- xportr_length(adsl, metadata, domain = "adsl", length_source = "metadata")
```

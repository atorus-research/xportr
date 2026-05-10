# Assign SAS Format

Assigns a SAS format from a variable level metadata to a given data
frame. If no format is found for a given variable, it is set as an empty
character vector. This is stored in the '`format.sas`' attribute.

## Usage

``` r
xportr_format(.df, metadata = NULL, domain = NULL, verbose = NULL)
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

Data frame with `SASformat` attributes for each variable.

## Format Checks

This function carries out a series of basic checks to ensure the formats
being applied make sense.

Note, the 'type' of message that is generated will depend on the value
passed to the `verbose` argument: with 'stop' producing an error, 'warn'
producing a warning, or 'message' producing a message. A value of 'none'
will not output any messages.

1.  If the variable has a suffix of `DT`, `DTM`, or `TM` excluding
    `ELTM` (indicating a numeric date/time variable) , then a message
    will be shown if there is no format associated with it.

2.  If a variable is character then a message will be shown if there is
    no `$` prefix in the associated format.

3.  If a variable is character then a message will be shown if the
    associated format has greater than 31 characters (excluding the
    `$`).

4.  If a variable is numeric then a message will be shown if there is a
    `$` prefix in the associated format.

5.  If a variable is numeric then a message will be shown if the
    associated format has greater than 32 characters.

6.  All formats will be checked against a list of formats considered
    'standard' as part of an ADaM dataset. Note, however, this list is
    not exhaustive (it would not be feasible to check all the functions
    within the scope of this package). If the format is not found in the
    'standard' list, then a message is created advising the user to
    check.

|                 |              |              |
|-----------------|--------------|--------------|
| **Format Name** | **w Values** | **d Values** |
| w.d             | 1 - 32       | ., 0 - 31    |
| \$w.            | 1 - 200      |              |
| DATEw.          | ., 5 - 11    |              |
| DATETIMEw.      | 7 - 40       |              |
| DDMMYYw.        | ., 2 - 10    |              |
| HHMM.           |              |              |
| MMDDYYw.        | ., 2 - 10    |              |
| TIMEw.          | ., 2 - 20    |              |
| WEEKDATEw.      | ., 3 - 37    |              |
| YYMMDDw.        | ., 2 - 10    |              |
| B8601DAw.       | ., 8 - 10    |              |
| B8601DTw.d      | ., 15 - 26   | ., 0 - 6     |
| B8601TM.        |              |              |
| IS8601DA.       |              |              |
| IS8601TM.       |              |              |
| E8601DAw.       | ., 10        |              |
| E8601DNw.       | ., 10        |              |
| E8601DTw.d      | ., 16 - 26   | ., 0 - 6     |
| E8601DXw.       | ., 20 - 35   |              |
| E8601LXw.       | ., 20 - 35   |              |
| E8601LZw.       | ., 9 - 20    |              |
| E8601TMw.d      | ., 8 - 15    | ., 0 - 6     |
| E8601TXw.       | ., 9 - 20    |              |
| E8601TZw.d      | ., 9 - 20    | ., 0 - 6     |

## Metadata

The argument passed in the 'metadata' argument can either be a metacore
object, or a data.frame containing the data listed below. If metacore is
used, no changes to options are required.

For data.frame 'metadata' arguments three columns must be present:

1.  Domain Name - passed as the 'xportr.domain_name' option. Default:
    "dataset". This is the column subset by the 'domain' argument in the
    function.

2.  Format Name - passed as the 'xportr.format_name' option. Default:
    "format". Character values to update the '`format.sas`' attribute of
    the column. This is passed to
    [`haven::write_xpt`](https://haven.tidyverse.org/reference/read_xpt.html)
    to note the format.

3.  Variable Name - passed as the 'xportr.variable_name' option.
    Default: "variable". This is used to match columns in '.df' argument
    and the metadata.

## Examples

``` r
adsl <- data.frame(
  USUBJID = c(1001, 1002, 1003),
  BRTHDT = c(1, 1, 2)
)

metadata <- data.frame(
  dataset = c("adsl", "adsl"),
  variable = c("USUBJID", "BRTHDT"),
  format = c(NA, "DATE9.")
)

adsl <- xportr_format(adsl, metadata, domain = "adsl")
```

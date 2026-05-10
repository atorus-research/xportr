# Coerce variable type

XPT v5 datasets only have data types of character and numeric.
`xportr_type()` attempts to collapse R classes to those two XPT types.
The 'xportr.character_types' option is used to explicitly collapse the
class of a column to character using
[`as.character()`](https://rdrr.io/r/base/character.html). Similarly,
'xportr.numeric_types' will collapse a column to a numeric type. (See
[`xportr_options()`](https://atorus-research.github.io/xportr/dev/reference/xportr_options.md)
for default values of these options.) If no type is passed for a
variable, it is assumed to be numeric and coerced with
[`as.numeric()`](https://rdrr.io/r/base/numeric.html).

## Usage

``` r
xportr_type(.df, metadata = NULL, domain = NULL, verbose = NULL)
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

Returns the modified table.

## Details

Certain care should be taken when using timing variables. R serializes
dates based on a reference date of 01/01/1970 where XPT uses 01/01/1960.
This can result in dates being 10 years off when outputting from R to
XPT if you're using a date class. For this reason, `xportr` will try to
determine what should happen with variables that appear to be used to
denote time.

## Messaging

`type_log()` is the primary messaging tool for `xportr_type()`. The
number of column types that mismatch the reported type in the metadata,
if any, is reported by `xportr_type()`. If there are any type
mismatches, and the 'verbose' argument is 'stop', 'warn', or 'message',
each mismatch will be detailed with the actual type in the data and the
type noted in the metadata.

## Metadata

The argument passed in the 'metadata' argument can either be a metacore
object, or a data.frame containing the data listed below. If metacore is
used, no changes to options are required.

For data.frame 'metadata' arguments four columns must be present:

1.  Domain Name - passed as the 'xportr.domain_name' option. Default:
    "dataset". This is the column subset by the 'domain' argument in the
    function.

2.  Variable Name - passed as the 'xportr.variable_name' option.
    Default: "variable". This is used to match columns in '.df' argument
    and the metadata.

3.  Variable Type - passed as the 'xportr.type_name'. Default: "type".
    This is used to note the XPT variable "type" options are numeric or
    character.

4.  (Option only) Character Types - The list of classes that should be
    explicitly coerced to a XPT Character type. Default: c( "character",
    "char", "text", "date", "posixct", "posixt", "datetime", "time",
    "partialdate", "partialtime", "partialdatetime",
    "incompletedatetime", "durationdatetime", "intervaldatetime")\`

5.  (Option only) Numeric Types - The list of classes that should be
    explicitly coerced to a XPT numeric type. Default: c("integer",
    "numeric", "num", "float")

## Examples

``` r
metadata <- data.frame(
  dataset = "test",
  variable = c("Subj", "Param", "Val", "NotUsed"),
  type = c("numeric", "character", "numeric", "character")
)

.df <- data.frame(
  Subj = as.character(c(123, 456, 789)),
  Different = c("a", "b", "c"),
  Val = c("1", "2", "3"),
  Param = c("param1", "param2", "param3")
)

df2 <- xportr_type(.df, metadata, "test")
#> 
#> ── Variable type mismatches found. ──
#> 
#> ✔ 2 variables coerced
```

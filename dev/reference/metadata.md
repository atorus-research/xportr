# Set variable specifications and domain

Sets metadata and/or domain for a dataset in a way that can be accessed
by other xportr functions. If used at the start of an xportr pipeline,
it removes the need to set metadata and domain at each step
individually. For details on the format of the metadata, see the
'Metadata' section for each function in question.

## Usage

``` r
xportr_metadata(.df, metadata = NULL, domain = NULL, verbose = NULL)
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

`.df` dataset with metadata and domain attributes set

## Examples

``` r
metadata <- data.frame(
  dataset = "test",
  variable = c("Subj", "Param", "Val", "NotUsed"),
  type = c("numeric", "character", "numeric", "character"),
  format = NA,
  order = c(1, 3, 4, 2)
)

adlb <- data.frame(
  Subj = as.character(123, 456, 789),
  Different = c("a", "b", "c"),
  Val = c("1", "2", "3"),
  Param = c("param1", "param2", "param3")
)

xportr_metadata(adlb, metadata, "test")
#>   Subj Different Val  Param
#> 1  123         a   1 param1
#> 2  123         b   2 param2
#> 3  123         c   3 param3

library(magrittr)

adlb %>%
  xportr_metadata(metadata, "test") %>%
  xportr_type() %>%
  xportr_order()
#> 
#> ── Variable type mismatches found. ──
#> 
#> ✔ 2 variables coerced
#> 
#> ── 1 variables not in spec and moved to end ──
#> 
#> ── 2 reordered in dataset ──
#> 
#> ── Variables in metadata not found in dataset. ──
#> 
#> ✔ 1 metadata variables skipped
#>   Subj  Param Val Different
#> 1  123 param1   1         a
#> 2  123 param2   2         b
#> 3  123 param3   3         c
```

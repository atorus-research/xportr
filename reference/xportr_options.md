# Get or set xportr options

There are two mechanisms for working with options for xportr. One is the
[`options()`](https://rdrr.io/r/base/options.html) function, which is
part of base R, and the other is the `xportr_options()` function, which
is in the xportr package. The reason for these two mechanisms is has to
do with legacy code and scoping.

The [`options()`](https://rdrr.io/r/base/options.html) function sets
options globally, for the duration of the R process. The
[`getOption()`](https://rdrr.io/r/base/options.html) function retrieves
the value of an option. All xportr related options of this type are
prefixed with `"xportr."`.

## Usage

``` r
xportr_options(...)
```

## Arguments

- ...:

  Options to set, with the form `name = value` or a character vector of
  option names.

## Options with [`options()`](https://rdrr.io/r/base/options.html)

- xportr.df_domain_name:

  defaults to `"dataset"`  
  The name of the domain "name" column in dataset metadata.

- xportr.df_label:

  defaults to `"label"`  
  The column noting the dataset label in dataset metadata.

- xportr.domain_name:

  defaults to `"dataset"`  
  The name of the domain "name" column in variable metadata.

- xportr.variable_name:

  defaults to `"variable"`  
  The name of the variable "name" in variable metadata.

- xportr.type_name:

  defaults to `"type"`  
  The name of the variable type column in variable metadata.

- xportr.label:

  defaults to `"label"`  
  The name of the variable label column in variable metadata.

- xportr.length:

  defaults to `"length"`  
  The name of the variable length column in variable metadata.

- xportr.order_name:

  defaults to `"order"`  
  The name of the variable order column in variable metadata.

- xportr.format_name:

  defaults to `"format"`  
  The name of the variable format column in variable metadata.

- xportr.format_verbose:

  defaults to `"none"`  
  The default argument for the 'verbose' argument for `xportr_format`.

- xportr.label_verbose:

  defaults to `"none"`  
  The default argument for the 'verbose' argument for `xportr_label`.

- xportr.length_verbose:

  defaults to `"none"`  
  The default argument for the 'verbose' argument for `xportr_length`.

- xportr.type_verbose:

  defaults to `"none"`  
  The default argument for the 'verbose' argument for `xportr_type`.

- xportr.order_verbose:

  defaults to `"none"`  
  The default argument for the 'verbose' argument for `xportr_order`.

- xportr.character_types:

  defaults to `"character"`  
  The default character vector used to explicitly coerce R classes to
  character XPT types.

- xportr.character_metadata_types:

  defaults to
  `c("character", "char", "text", "date", "posixct", "posixt", "datetime", "time", "partialdate", "partialtime", "partialdatetime", "incompletedatetime", "durationdatetime", "intervaldatetime")`  
  The default character vector used to explicitly coerce R classes to
  character XPT types.

- xportr.numeric_metadata_types:

  defaults to `c("integer", "numeric", "num", "float")`  
  The default character vector used to explicitly coerce R classes to
  numeric XPT types.

- xportr.numeric_types:

  defaults to
  `c("integer", "float", "numeric", "posixct", "posixt", "time", "date", "hms")`  
  The default character vector used to explicitly coerce R classes to
  numeric XPT types.

## Options with `xportr_options()`

Alternative to the [`options()`](https://rdrr.io/r/base/options.html),
the `xportr_options()` function can be used to set the options. The
`xportr_options()` function also returns the current options when a
character vector of the options keys are passed into it. If nothing is
passed into it, it returns the state of all xportr options.

## Examples

``` r
xportr_options("xportr.df_label")
#> $xportr.df_label
#> [1] "label"
#> 
xportr_options(xportr.df_label = "data_label", xportr.label = "custom_label")
xportr_options(c("xportr.label", "xportr.df_label"))
#> $xportr.label
#> [1] "custom_label"
#> 
#> $xportr.df_label
#> [1] "data_label"
#> 
xportr_options()
#> $xportr.df_domain_name
#> [1] "dataset"
#> 
#> $xportr.df_label
#> [1] "data_label"
#> 
#> $xportr.domain_name
#> [1] "dataset"
#> 
#> $xportr.variable_name
#> [1] "variable"
#> 
#> $xportr.type_name
#> [1] "type"
#> 
#> $xportr.label
#> [1] "custom_label"
#> 
#> $xportr.length
#> [1] "length"
#> 
#> $xportr.order_name
#> [1] "order"
#> 
#> $xportr.format_name
#> [1] "format"
#> 
#> $xportr.format_verbose
#> [1] "none"
#> 
#> $xportr.label_verbose
#> [1] "none"
#> 
#> $xportr.length_verbose
#> [1] "none"
#> 
#> $xportr.type_verbose
#> [1] "none"
#> 
#> $xportr.order_verbose
#> [1] "none"
#> 
#> $xportr.character_types
#> [1] "character"
#> 
#> $xportr.character_metadata_types
#>  [1] "character"          "char"               "text"              
#>  [4] "date"               "posixct"            "posixt"            
#>  [7] "datetime"           "time"               "partialdate"       
#> [10] "partialtime"        "partialdatetime"    "incompletedatetime"
#> [13] "durationdatetime"   "intervaldatetime"  
#> 
#> $xportr.numeric_metadata_types
#> [1] "integer" "numeric" "num"     "float"  
#> 
#> $xportr.numeric_types
#> [1] "integer" "float"   "numeric" "posixct" "posixt"  "time"    "date"   
#> [8] "hms"    
#> 
```

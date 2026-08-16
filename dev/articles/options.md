# You got options

## Introduction

Before we dive into the
[xportr](https://atorus-research.github.io/xportr/) functions, we want
to point out some quality of life utilities to make your `xpt`
generation life a little bit easier.

- [`options()`](https://rdrr.io/r/base/options.html)
- [`xportr_options()`](https://atorus-research.github.io/xportr/dev/reference/xportr_options.md)

**NOTE:** As long as you have a well-defined *metadata object* you do
NOT need to use [`options()`](https://rdrr.io/r/base/options.html) or
[`xportr_options()`](https://atorus-research.github.io/xportr/dev/reference/xportr_options.md),
but we find these handy to use and think they deserve a quick mention!

R has a built-in mechanism for storing global settings called
[`options()`](https://rdrr.io/r/base/options.html). You set them with
[`options()`](https://rdrr.io/r/base/options.html) and retrieve a single
value with [`getOption()`](https://rdrr.io/r/base/options.html). Options
live for the duration of your R session - once set, any function that
reads that option will pick up the new value without you having to pass
it as an argument every time. All
[xportr](https://atorus-research.github.io/xportr/) options are prefixed
with `xportr.` to avoid clashing with options from other packages.

We will also explore the following in this vignette:

- You’ve got [`options()`](https://rdrr.io/r/base/options.html) or
  [`xportr_options()`](https://atorus-research.github.io/xportr/dev/reference/xportr_options.md)
  — column name mapping
- Are we being too verbose? — verbose messaging defaults
- Type coercion options
- Putting it all together

## You’ve got `options()` or `xportr_options()`

[xportr](https://atorus-research.github.io/xportr/) is built with
certain assumptions around specification column names and information in
those columns. We have found that each company specification file can
differ slightly from our assumptions. For example, one company might
call a column `Variables`, another `Variable` and another `variables`.
Rather than trying to regex ourselves out of this situation, we have
introduced [`options()`](https://rdrr.io/r/base/options.html).

Additionally, we have a helper function
[`xportr_options()`](https://atorus-research.github.io/xportr/dev/reference/xportr_options.md),
which works just like [`options()`](https://rdrr.io/r/base/options.html)
but it can also be used to get the current state of all
[xportr](https://atorus-research.github.io/xportr/) options — we will
use this at the end of the vignette.

``` r
library(xportr)
```

Let’s look at our example specification file column names available in
this package. We can see that all the columns start with an upper case
letter and have spaces in several of them. We could convert all the
column names to lower case and deal with the spacing using some
[dplyr](https://dplyr.tidyverse.org) functions or base R, or we could
just use [`options()`](https://rdrr.io/r/base/options.html)!

``` r
data("adsl_xportr", "var_spec", "dataset_spec", package = "xportr")
colnames(var_spec)
   [1] "Order"              "Dataset"            "Variable"          
   [4] "Label"              "Data Type"          "Length"            
   [7] "Significant Digits" "Format"             "Mandatory"         
  [10] "Assigned Value"     "Codelist"           "Common"            
  [13] "Origin"             "Pages"              "Method"            
  [16] "Predecessor"        "Role"               "Comment"           
  [19] "Developer Notes"
```

By using [`options()`](https://rdrr.io/r/base/options.html) or
[`xportr_options()`](https://atorus-research.github.io/xportr/dev/reference/xportr_options.md)
at the beginning of our script we can tell
[xportr](https://atorus-research.github.io/xportr/) what the valid names
are (see chunk below). Please note that before we set the options the
package assumed everything was in lowercase and there were no spaces in
the names. After running
[`options()`](https://rdrr.io/r/base/options.html) or
[`xportr_options()`](https://atorus-research.github.io/xportr/dev/reference/xportr_options.md),
[xportr](https://atorus-research.github.io/xportr/) sees the column
`Variable` as the valid name rather than `variable`. You can inspect
[`?xportr_options`](https://atorus-research.github.io/xportr/dev/reference/xportr_options.md)
to look at additional options.

``` r
xportr_options(
  xportr.variable_name = "Variable",
  xportr.label = "Label",
  xportr.type_name = "Data Type",
  xportr.format_name = "Format",
  xportr.length = "Length",
  xportr.order_name = "Order"
)

# Or alternatively
options(
  xportr.variable_name = "Variable",
  xportr.label = "Label",
  xportr.type_name = "Data Type",
  xportr.format_name = "Format",
  xportr.length = "Length",
  xportr.order_name = "Order"
)
```

Below is the full list of column name options and their defaults.

| Option                  | Default      | Controls                                        |
|-------------------------|--------------|-------------------------------------------------|
| `xportr.domain_name`    | `"dataset"`  | Domain/dataset name column in variable metadata |
| `xportr.variable_name`  | `"variable"` | Variable name column                            |
| `xportr.type_name`      | `"type"`     | Variable type column                            |
| `xportr.label`          | `"label"`    | Variable label column                           |
| `xportr.length`         | `"length"`   | Variable length column                          |
| `xportr.order_name`     | `"order"`    | Variable order column                           |
| `xportr.format_name`    | `"format"`   | Variable format column                          |
| `xportr.df_domain_name` | `"dataset"`  | Domain name column in dataset metadata          |
| `xportr.df_label`       | `"label"`    | Dataset label column in dataset metadata        |

## Are we being too verbose?

One final note on the options. Five of the core
[xportr](https://atorus-research.github.io/xportr/) functions have the
ability to set messaging as `"none"`, `"message"`, `"warn"`, `"stop"`.
Setting each of these in all your calls can be a bit repetitive. You can
use [`options()`](https://rdrr.io/r/base/options.html) or
[`xportr_options()`](https://atorus-research.github.io/xportr/dev/reference/xportr_options.md)
to set these at a higher level and avoid this repetition.

| Value       | Behavior                                                           |
|-------------|--------------------------------------------------------------------|
| `"none"`    | Disables any messaging, keeping the console output clean (default) |
| `"message"` | Sends a standard message to the console                            |
| `"warn"`    | Sends a warning message to the console                             |
| `"stop"`    | Stops execution and sends an error message to the console          |

``` r
# Default verbose is set to `none`
xportr_options(
  xportr.type_verbose = "none",
  xportr.label_verbose = "none",
  xportr.length_verbose = "none",
  xportr.order_verbose = "none",
  xportr.format_verbose = "none"
)

xportr_options(
  xportr.type_verbose = "message", # Sends a standard message to the console
  xportr.label_verbose = "message",
  xportr.length_verbose = "warn", # Sends a warning message to the console
  xportr.order_verbose = "warn",
  xportr.format_verbose = "stop" # Stops execution and sends an error message to the console
)
```

Note that any per-call `verbose` argument still overrides the option, so
you can always tighten or loosen the level for a specific call without
changing the global default.

## Type coercion options

[xportr](https://atorus-research.github.io/xportr/) also needs to know
which R classes map to character XPT types and which map to numeric XPT
types. These are unlikely to need changing unless your specification
file uses non-standard type labels (e.g., `"INT"` instead of
`"integer"`), but they are configurable if needed.

| Option                            | Default                                             | Controls                                        |
|-----------------------------------|-----------------------------------------------------|-------------------------------------------------|
| `xportr.character_types`          | `"character"`                                       | R classes treated as character in type coercion |
| `xportr.character_metadata_types` | `"character"`, `"char"`, `"text"`, `"date"`, …      | Metadata type strings mapped to character XPT   |
| `xportr.numeric_types`            | `"integer"`, `"float"`, `"numeric"`, `"posixct"`, … | R classes treated as numeric in type coercion   |
| `xportr.numeric_metadata_types`   | `"integer"`, `"numeric"`, `"num"`, `"float"`        | Metadata type strings mapped to numeric XPT     |

``` r
# Tell xportr that "INT" in your spec means integer/numeric
xportr_options(
  xportr.numeric_metadata_types = c("integer", "numeric", "num", "float", "INT")
)
```

## Putting it all together

A typical script might start with a single
[`xportr_options()`](https://atorus-research.github.io/xportr/dev/reference/xportr_options.md)
block that configures everything up front. After that, all
[xportr](https://atorus-research.github.io/xportr/) calls pick up the
settings automatically — no need to repeat yourself in every function
call!

``` r
library(xportr)

xportr_options(
  # Column name mapping for our spec file
  xportr.variable_name = "Variable",
  xportr.label = "Label",
  xportr.type_name = "Data Type",
  xportr.format_name = "Format",
  xportr.length = "Length",
  xportr.order_name = "Order",
  # Messaging preferences
  xportr.type_verbose = "message",
  xportr.label_verbose = "message",
  xportr.length_verbose = "warn",
  xportr.order_verbose = "warn",
  xportr.format_verbose = "none"
)

ADSL |>
  xportr_metadata(var_spec, "ADSL") |>
  xportr_type() |>
  xportr_length(length_source = "metadata") |>
  xportr_label() |>
  xportr_order() |>
  xportr_format() |>
  xportr_df_label(dataset_spec) |>
  xportr_write("adsl.xpt")
```

You can confirm what is currently set at any time by calling
[`xportr_options()`](https://atorus-research.github.io/xportr/dev/reference/xportr_options.md)
with no arguments, or use base R’s
[`getOption()`](https://rdrr.io/r/base/options.html) for a single value:

``` r
xportr_options()
  $xportr.df_domain_name
  [1] "dataset"
  
  $xportr.df_label
  [1] "label"
  
  $xportr.domain_name
  [1] "dataset"
  
  $xportr.variable_name
  [1] "variable"
  
  $xportr.type_name
  [1] "type"
  
  $xportr.label
  [1] "label"
  
  $xportr.length
  [1] "length"
  
  $xportr.order_name
  [1] "order"
  
  $xportr.format_name
  [1] "format"
  
  $xportr.format_verbose
  [1] "none"
  
  $xportr.label_verbose
  [1] "none"
  
  $xportr.length_verbose
  [1] "none"
  
  $xportr.type_verbose
  [1] "none"
  
  $xportr.order_verbose
  [1] "none"
  
  $xportr.character_types
  [1] "character"
  
  $xportr.character_metadata_types
   [1] "character"          "char"               "text"              
   [4] "date"               "posixct"            "posixt"            
   [7] "datetime"           "time"               "partialdate"       
  [10] "partialtime"        "partialdatetime"    "incompletedatetime"
  [13] "durationdatetime"   "intervaldatetime"  
  
  $xportr.numeric_metadata_types
  [1] "integer" "numeric" "num"     "float"  
  
  $xportr.numeric_types
  [1] "integer" "float"   "numeric" "posixct" "posixt"  "time"    "date"   
  [8] "hms"
getOption("xportr.label")
  [1] "label"
getOption("xportr.type_verbose")
  [1] "none"
```

Options persist for the life of your R session. To reset a single option
back to its default, set it explicitly:

``` r
options(xportr.label = "label")

# Or equivalently
xportr_options(xportr.label = "label")
```

To reset **all** xportr options at once, restart your R session or use
[`withr::with_options()`](https://withr.r-lib.org/reference/with_options.html)
to scope changes to a block of code without permanently affecting the
global state.

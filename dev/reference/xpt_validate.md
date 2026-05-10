# Validate Dataset Can be Written to xpt

Function used to validate dataframes before they are sent to
[`haven::write_xpt`](https://haven.tidyverse.org/reference/read_xpt.html)
for writing.

## Usage

``` r
xpt_validate(data)
```

## Arguments

- data:

  Dataset to be exported as xpt file

## Value

Returns a character vector of failed conditions

## Details

`xpt_validate()` performs four focused checks before
[`xportr_write()`](https://atorus-research.github.io/xportr/dev/reference/xportr_write.md)
attempts to create an XPT file:

- **Variable names** – maximum of 8 characters, must start with a
  letter, use only ASCII alphanumeric characters (no underscores or
  symbols), and remain uppercase.

- **Variable labels** – maximum of 40 characters and limited to ASCII
  printable characters.

- **Formats** – SAS format attributes must match the internal allow-list
  or follow a `w.d` pattern such as `8.` or `12.3`.

- **Character data lengths** – each character column's maximum byte
  length cannot exceed 200.

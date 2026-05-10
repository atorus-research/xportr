# Minimal data frame mock of a valid ADaM dataset

This function is only used in tests.

## Usage

``` r
minimal_table(n_rows = 3, cols = c("x", "y"))
```

## Arguments

- n_rows:

  Numeric value that indicates the number of rows of the data frame

- cols:

  Vector of characters that indicates which columns to return. By
  default only `x` and `y` are returned with numeric contents.

## Value

A data.frame mimicking a valid ADaM dataset.

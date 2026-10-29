# Minimal metadata data frame mock for a ADaM dataset

Minimal metadata data frame mock for a ADaM dataset

## Usage

``` r
minimal_metadata(
  dataset = FALSE,
  length = FALSE,
  label = FALSE,
  type = FALSE,
  format = FALSE,
  order = FALSE,
  dataset_name = "adsl",
  var_names = NULL
)
```

## Arguments

- dataset:

  Flag that indicates that the `dataset` column should be included.

- length:

  Flag that indicates that the `length` column should be included.

- label:

  Flag that indicates that the `label` column should be included.

- type:

  Flag that indicates that the `type` column should be included.

- format:

  Flag that indicates that the `format` column should be included.

- order:

  Flag that indicates that the `order` column should be included.

- dataset_name:

  String with name of domain.

- var_names:

  Character vector that defines which variables (rows) to keep

## Value

A metadata data.frame

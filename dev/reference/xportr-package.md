# The `xportr` package

`xportr` is designed to be a clinical workflow friendly method for
outputting CDISC complaint data sets in R, to XPT version 5 files. It
was designed with options in mind to allow for flexible setting of
options while allowing projects and system administrators to set
sensible defaults for their organizations workflows. Below are a list of
options that can be set to customize how `xportr` works in your
environment.

## xportr options

- xportr.df_domain_name - The name of the domain "name" column in
  dataset metadata. Default: `"dataset"`

- xportr.df_label - The column noting the dataset label in dataset
  metadata. Default: `"label"`

- xportr.domain_name - The name of the domain "name" column in variable
  metadata. Default: `"dataset"`

- xportr.variable_name - The name of the variable "name" in variable
  metadata. Default: `"variable"`

- xportr.type_name - The name of the variable type column in variable
  metadata. Default: `"type"`

- xportr.label - The name of the variable label column in variable
  metadata. Default: `"label"`

- xportr.length - The name of the variable length column in variable
  metadata. Default: `"length"`

- xportr.order_name - The name of the variable order column in variable
  metadata. Default: `"order"`

- xportr.format_name - The name of the variable format column in
  variable metadata. Default: `"format"`

- xportr.format_verbose - The default argument for the 'verbose'
  argument for `xportr_format`. Default: `"none"`

- xportr.label_verbose - The default argument for the 'verbose' argument
  for `xportr_label`. Default: `"none"`

- xportr.length_verbose - The default argument for the 'verbose'
  argument for `xportr_length`. Default: `"none"`

- xportr.type_verbose - The default argument for the 'verbose' argument
  for `xportr_type`. Default: `"none"`

- xportr.order_verbose - The default argument for the 'verbose' argument
  for `xportr_order`. Default: `"none"`

- xportr.character_types - The default character vector used to
  explicitly coerce R classes to character XPT types. Default:
  `"character"`

- xportr.character_metadata_types - The default character vector used to
  explicitly coerce R classes to character XPT types. Default:
  `c("character", "char", "text", "date", "posixct", "posixt", "datetime", "time", "partialdate", "partialtime", "partialdatetime", "incompletedatetime", "durationdatetime", "intervaldatetime")`

- xportr.numeric_metadata_types - The default character vector used to
  explicitly coerce R classes to numeric XPT types. Default:
  `c("integer", "numeric", "num", "float")`

- xportr.numeric_types - The default character vector used to explicitly
  coerce R classes to numeric XPT types. Default:
  `c("integer", "float", "numeric", "posixct", "posixt", "time", "date", "hms")`

## Updating Options

- For a single session, an option can be changed by
  `options(<optionToChange> = <NewValue>)`.

- To change an option for a single projects across sessions in that
  projects, place the options update in the `.Rprofile` in that project
  directory.

- To change an option for a user across all sessions, place the options
  update in the `.Rprofile` file in the users home directory.

- To change an option for all users in an R environment, place the
  options update in the `.Rprofile.site` file in the R home directory.

## See also

Useful links:

- <https://atorus-research.github.io/xportr/>

- <https://github.com/atorus-research/xportr>

- Report bugs at <https://github.com/atorus-research/xportr/issues>

## Author

**Maintainer**: Eli Miller <Eli.Miller@AtorusResearch.com>
([ORCID](https://orcid.org/0000-0002-2127-9456))

Authors:

- Ben Straub

- Zelos Zhu

- Ethan Brockmann

- Vedha Viyash

- Andre Verissimo

- Sophie Shapcott

- Celine Piraux

- Kangjie Zhang

- Adrian Chan

- Sadchla Mascary

- Seunghyun Kim

Other contributors:

- Atorus/GSK JPT \[copyright holder\]

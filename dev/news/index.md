# Changelog

## xportr 0.5.0.9000

### New Features

- Added internal `group_data_check()` function to check and warn users
  of data grouping in their data when using xportr functions.
  ([\#270](https://github.com/atorus-research/xportr/issues/270))
  ([\#297](https://github.com/atorus-research/xportr/issues/297))
- Added a check in
  [`xpt_validate()`](https://atorus-research.github.io/xportr/dev/reference/xpt_validate.md)
  to verify the class of date, datetime, and time variables.
  ([\#298](https://github.com/atorus-research/xportr/issues/298))
- Added check to
  [`xportr_label()`](https://atorus-research.github.io/xportr/dev/reference/xportr_label.md),
  [`xportr_length()`](https://atorus-research.github.io/xportr/dev/reference/xportr_length.md),
  and
  [`xportr_order()`](https://atorus-research.github.io/xportr/dev/reference/xportr_order.md)
  to alert users when metadata contains variables that are not present
  in the dataset
  ([\#284](https://github.com/atorus-research/xportr/issues/284))

### Bug Fixes

- Fixed verbose option bugs in
  [`xportr_format()`](https://atorus-research.github.io/xportr/dev/reference/xportr_format.md)
  and added missing `xportr.order_verbose` option
  ([\#318](https://github.com/atorus-research/xportr/issues/318))
- Fixed
  [`xportr_format()`](https://atorus-research.github.io/xportr/dev/reference/xportr_format.md)
  to exclude variables ending with `ELTM` from the date/time format
  check, as these indicate time relative to an anchor time in SDTM or
  ADaM ([\#293](https://github.com/atorus-research/xportr/issues/293))

### Breaking Changes and Deprecation

- The deprecated `metacore` argument has been removed from
  [`xportr_type()`](https://atorus-research.github.io/xportr/dev/reference/xportr_type.md),
  [`xportr_label()`](https://atorus-research.github.io/xportr/dev/reference/xportr_label.md),
  [`xportr_length()`](https://atorus-research.github.io/xportr/dev/reference/xportr_length.md),
  [`xportr_order()`](https://atorus-research.github.io/xportr/dev/reference/xportr_order.md),
  [`xportr_df_label()`](https://atorus-research.github.io/xportr/dev/reference/xportr_df_label.md),
  and
  [`xportr_format()`](https://atorus-research.github.io/xportr/dev/reference/xportr_format.md)
  functions. Use the `metadata` argument instead.
  ([\#317](https://github.com/atorus-research/xportr/issues/317))
- The deprecated `label` argument has been removed from
  [`xportr_write()`](https://atorus-research.github.io/xportr/dev/reference/xportr_write.md).
  Use the `metadata` argument instead.
  ([\#317](https://github.com/atorus-research/xportr/issues/317))

### Documentation

### Miscellaneous

- Standardized function calls by centralizing imports in
  R/xportr-package.R, replacing inconsistent use of package::function()
  syntax. (see [Conventions in
  xportr](https://github.com/atorus-research/xportr/wiki/Conventions-in-xportr)).
  ([\#204](https://github.com/atorus-research/xportr/issues/204))

- Updated warning when trying to write and file size exceeds 5GB
  ([\#288](https://github.com/atorus-research/xportr/issues/288))

## xportr 0.4.3

CRAN release: 2025-07-15

- Fix breaking changes in metacore.
  ([\#290](https://github.com/atorus-research/xportr/issues/290))

## xportr 0.4.2

CRAN release: 2025-01-07

- Added logging to the `domain` argument in `xportr` functions to notify
  user if the domain passed doesn’t exist in the metadata.
  ([\#260](https://github.com/atorus-research/xportr/issues/260))

- Updated order messaging to clarify some messaging when all data in
  dataset is found in the specification.
  ([\#269](https://github.com/atorus-research/xportr/issues/269))

- `"hms"` was added to the default value of the `xportr.numeric_types`
  option. This ensures that
  [xportr](https://atorus-research.github.io/xportr/) works smoothly
  with variables created by `admiral::derive_vars_dtm_to_tm()`.
  ([\#271](https://github.com/atorus-research/xportr/issues/271))

- More details were added to the messages of
  [`xportr_type()`](https://atorus-research.github.io/xportr/dev/reference/xportr_type.md).
  ([\#271](https://github.com/atorus-research/xportr/issues/271))

## xportr 0.4.1

CRAN release: 2024-10-07

### New Feature

- New argument in
  [`xportr_write()`](https://atorus-research.github.io/xportr/dev/reference/xportr_write.md)
  allows users to specify the maximum file size (in GB) of their
  exported xpt files.
  ([\#268](https://github.com/atorus-research/xportr/issues/268))
- [`xportr_split()`](https://atorus-research.github.io/xportr/dev/reference/xportr_split.md)
  is deprecated with this new argument above now added to
  [`xportr_write()`](https://atorus-research.github.io/xportr/dev/reference/xportr_write.md)

## xportr 0.4.0

CRAN release: 2024-03-28

### New Features

- All core functions can be run together by using new function
  [`xportr()`](https://atorus-research.github.io/xportr/dev/reference/xportr.md)
  ([\#137](https://github.com/atorus-research/xportr/issues/137))
- [`xportr_metadata()`](https://atorus-research.github.io/xportr/dev/reference/metadata.md)
  can set `verbose` for a whole pipeline, i.e. setting `verbose` in
  [`xportr_metadata()`](https://atorus-research.github.io/xportr/dev/reference/metadata.md)
  will populate to all `xportr` functions.
  ([\#151](https://github.com/atorus-research/xportr/issues/151))
- [`xportr_split()`](https://atorus-research.github.io/xportr/dev/reference/xportr_split.md)
  is a new function that allows users to split a dataset into multiple
  output files based on a variable.
  ([\#183](https://github.com/atorus-research/xportr/issues/183))
- [`xportr_write()`](https://atorus-research.github.io/xportr/dev/reference/xportr_write.md)
  now accepts `metadata` argument which can be used to set the dataset
  label to stay consistent with the other `xportr_*` functions. It is
  noteworthy that the dataset label set using the
  [`xportr_df_label()`](https://atorus-research.github.io/xportr/dev/reference/xportr_df_label.md)
  function will be retained during the
  [`xportr_write()`](https://atorus-research.github.io/xportr/dev/reference/xportr_write.md).
  ([\#179](https://github.com/atorus-research/xportr/issues/179))
- Added a check for character variable lengths up to 200 bytes in
  [`xpt_validate()`](https://atorus-research.github.io/xportr/dev/reference/xpt_validate.md)([\#91](https://github.com/atorus-research/xportr/issues/91),
  [\#189](https://github.com/atorus-research/xportr/issues/189)).
- File name check is moved to strict_checks condition to allow
  underscores in the file name. Underscores are allowed in xpt but not
  per FDA requirements.
  ([\#126](https://github.com/atorus-research/xportr/issues/126))
- It is now possible to get and set the xportr options using the helper
  function
  [`xportr_options()`](https://atorus-research.github.io/xportr/dev/reference/xportr_options.md)
  ([\#130](https://github.com/atorus-research/xportr/issues/130))
- Added `xportr.character_metadata_types` and
  `xportr.numeric_metadata_types` to list the metadata types that are
  character or numeric. Updated `xportr.character_types` and
  `xportr.numeric_types` to list only the R types that are character and
  the R types that are numeric. This ensures that all R types, including
  dates, are now managed by xportr_type. If the R type differs from the
  metadata type, the variable is coerced
  ([\#161](https://github.com/atorus-research/xportr/issues/161)).
- New argument in
  [`xportr_length()`](https://atorus-research.github.io/xportr/dev/reference/xportr_length.md)
  allows selection between the length from metadata, as previously done,
  or from the calculated maximum length per variable when
  `length_source` is set to “data”
  ([\#91](https://github.com/atorus-research/xportr/issues/91))
- Series of basic checks added to the
  [`xportr_format()`](https://atorus-research.github.io/xportr/dev/reference/xportr_format.md)
  function to ensure format lengths, prefixes are accurate for the
  variable type. Also to ensure that any numeric date/datetime/time
  variables have a format.
  ([\#164](https://github.com/atorus-research/xportr/issues/164))
- [`xportr_length()`](https://atorus-research.github.io/xportr/dev/reference/xportr_length.md)
  assigns the maximum length value instead of 200 for a character
  variable when the length is missing in the metadata
  ([\#207](https://github.com/atorus-research/xportr/issues/207))

### Bug Fixes

- Bug fix for domain filtering
  ([\#137](https://github.com/atorus-research/xportr/issues/137))
- Make
  [`xportr_type()`](https://atorus-research.github.io/xportr/dev/reference/xportr_type.md)
  drop factor levels when coercing variables
  ([\#159](https://github.com/atorus-research/xportr/issues/159))

### Deprecation and Breaking Changes

- The `domain` argument for xportr functions will no longer be
  dynamically determined by the name of the data frame passed as the
  `.df` argument. This was done to make the use of xportr functions more
  explicit.
  ([\#182](https://github.com/atorus-research/xportr/issues/182))
- The `metacore` argument, which was renamed to `metadata` in the
  following six xportr functions:
  ([`xportr_df_label()`](https://atorus-research.github.io/xportr/dev/reference/xportr_df_label.md),
  [`xportr_format()`](https://atorus-research.github.io/xportr/dev/reference/xportr_format.md),
  [`xportr_label()`](https://atorus-research.github.io/xportr/dev/reference/xportr_label.md),
  [`xportr_length()`](https://atorus-research.github.io/xportr/dev/reference/xportr_length.md),
  [`xportr_order()`](https://atorus-research.github.io/xportr/dev/reference/xportr_order.md),
  and
  [`xportr_type()`](https://atorus-research.github.io/xportr/dev/reference/xportr_type.md))
  in version `0.3.0` with a soft deprecation warning, has now been hard
  deprecated. Please update your code to use the new `metadata` argument
  in place of `metacore`.
  ([\#203](https://github.com/atorus-research/xportr/issues/203))
- `SASlength` and `SAStype` were removed since they did not have an
  impact on `xpt_validate` or any other functions
  ([\#132](https://github.com/atorus-research/xportr/issues/132))
- `adsl` data object is now called `adsl_xportr`
  ([\#237](https://github.com/atorus-research/xportr/issues/237))

### Documentation

- Created development version of the website
  ([\#187](https://github.com/atorus-research/xportr/issues/187))
- Additional guidance for options added in deep dive vignette
  ([\#81](https://github.com/atorus-research/xportr/issues/81))
- Added a vignette about standards in different agencies
  ([\#206](https://github.com/atorus-research/xportr/issues/206))
- Removed non-user facing function documentation
  ([\#192](https://github.com/atorus-research/xportr/issues/192))
- Added more details about
  [`xportr_length()`](https://atorus-research.github.io/xportr/dev/reference/xportr_length.md)
  and
  [`xportr()`](https://atorus-research.github.io/xportr/dev/reference/xportr.md)
  to deep dive vignette
  ([\#215](https://github.com/atorus-research/xportr/issues/215),
  [\#222](https://github.com/atorus-research/xportr/issues/222))

### Miscellaneous

- Tests use [withr](https://withr.r-lib.org) to create temporary files
  that are automatically deleted
  ([\#219](https://github.com/atorus-research/xportr/issues/219))
- Remove unused packages from Suggests
  ([\#221](https://github.com/atorus-research/xportr/issues/221),
  [\#237](https://github.com/atorus-research/xportr/issues/237))
- Exporting a new dataset `dataset_spec` that contains the Dataset
  Specification for ADSL.
  ([\#179](https://github.com/atorus-research/xportr/issues/179))
- Adds argument assertions to public functions using
  [checkmate](https://mllg.github.io/checkmate/)
  ([\#175](https://github.com/atorus-research/xportr/issues/175))
- Data objects are no longer lazy loaded, which means that when needed
  the user must call `data("name_of_object")` first
  ([\#237](https://github.com/atorus-research/xportr/issues/237))

## xportr 0.3.2

CRAN release: 2024-02-19

- Removed unused packages, [tm](https://tm.r-forge.r-project.org/) and
  [janitor](https://github.com/sfirke/janitor) from Imports
  ([\#241](https://github.com/atorus-research/xportr/issues/241))

## xportr 0.3.1

CRAN release: 2023-09-14

- Fixed issues around code coverage
  ([\#170](https://github.com/atorus-research/xportr/issues/170)) and
  `lintr`
  ([\#176](https://github.com/atorus-research/xportr/issues/176))

## xportr 0.3.0

CRAN release: 2023-06-21

### New Features and Bug Fixes

- Fixed an issue where
  [`xportr_type()`](https://atorus-research.github.io/xportr/dev/reference/xportr_type.md)
  would overwrite column labels, widths, and “sas.formats”
- Fixed messaging of
  [`xportr_order()`](https://atorus-research.github.io/xportr/dev/reference/xportr_order.md)to
  give better visibility of the number of variables being reordered.
- Add new argument to
  [`xportr_write()`](https://atorus-research.github.io/xportr/dev/reference/xportr_write.md)
  to allow users to specify how xpt validation checks are handled.
- Fixed bug where character_types were case sensitive. They are now case
  insensitive
  ([\#77](https://github.com/atorus-research/xportr/issues/77)).
- Updated
  [`xportr_type()`](https://atorus-research.github.io/xportr/dev/reference/xportr_type.md)
  to make type coercion more explicit.
- `xpt_validate` updated to accept iso8601 date formats.
  ([\#76](https://github.com/atorus-research/xportr/issues/76))
- Added function
  [`xportr_metadata()`](https://atorus-research.github.io/xportr/dev/reference/metadata.md)
  to explicitly set metadata at the start of a pipeline
  ([\#44](https://github.com/atorus-research/xportr/issues/44))
- Metadata order columns are now coerced to numeric by default in
  [`xportr_order()`](https://atorus-research.github.io/xportr/dev/reference/xportr_order.md)
  to prevent character sorting
  ([\#149](https://github.com/atorus-research/xportr/issues/149))
- Message is shown on `xportr_*` functions when the metadata being used
  has multiple variables with the same name in the same domain
  ([\#128](https://github.com/atorus-research/xportr/issues/128))
- Fixed an issue with `xport_type()` where `DT`, `DTM` variables with a
  format specified in the metadata (e.g. `date9.`, `datetime20.`) were
  being converted to numeric, which will cause a 10 year difference when
  reading it back by `read_xpt()`. SAS’s uniform start date is 1960
  whereas Linux’s uniform start date is 1970
  ([\#142](https://github.com/atorus-research/xportr/issues/142)).
- Fixed an issue with R’s pipe `|>` that was causing functions to abort
  ([\#97](https://github.com/atorus-research/xportr/issues/97))
- Removed `<` and `>` as illegal characters in variable and dataset
  labels ([\#98](https://github.com/atorus-research/xportr/issues/98))

### Documentation

- Moved [pkgdown](https://pkgdown.r-lib.org/) site to bootswatch.
  Enabled search and linked slack icon
  ([\#122](https://github.com/atorus-research/xportr/issues/122)).
- Additional Deep Dive vignette showcasing functions and quality of life
  utilities for processing `xpts` created
  ([\#84](https://github.com/atorus-research/xportr/issues/84))
- Get Started vignette spruced up. Messages are now displayed and link
  to Deep Dive vignette
  ([\#150](https://github.com/atorus-research/xportr/issues/150))
- Increase test coverage to 100%
  ([\#82](https://github.com/atorus-research/xportr/issues/82))

### Deprecation and Breaking Changes

- The `metacore` argument has been renamed to `metadata` in the
  following six xportr functions:
  [`xportr_df_label()`](https://atorus-research.github.io/xportr/dev/reference/xportr_df_label.md),
  [`xportr_format()`](https://atorus-research.github.io/xportr/dev/reference/xportr_format.md),
  [`xportr_label()`](https://atorus-research.github.io/xportr/dev/reference/xportr_label.md),
  [`xportr_length()`](https://atorus-research.github.io/xportr/dev/reference/xportr_length.md),
  [`xportr_order()`](https://atorus-research.github.io/xportr/dev/reference/xportr_order.md),
  and
  [`xportr_type()`](https://atorus-research.github.io/xportr/dev/reference/xportr_type.md).
  Please update your code to use the new `metadata` argument in place of
  `metacore`.

## xportr 0.2.0

CRAN release: 2023-02-23

- Added a new validation test that errors when users pass invalid
  formats ([\#60](https://github.com/atorus-research/xportr/issues/60)
  [\#64](https://github.com/atorus-research/xportr/issues/64)). Thanks
  to [@zdz2101](https://github.com/zdz2101)!
- Fixed an issue where xportr_format could pass invalid formats to
  haven::write_xpt.

## xportr 0.1.0

CRAN release: 2022-06-21

Beta release for xportr

- Added exported functions `xportr_varnames` and `xportr_tidy_rename`
  into `dev` folder found on GitHub Repostiory. Intention to move into
  packages after CRAN release.
- Fixed xportr_format() bug
- Using admiral ADSL dataset in examples

## xportr 0.0.0.9000

Initial alpha release of xportr

- Development of 5 core functions
- Package down site and documentation created

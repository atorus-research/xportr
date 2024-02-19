# xportr 0.3.2

* Removed unused packages, `{tm}` and `{janitor}` from Imports (#241)

# xportr 0.3.1

* Fixed issues around code coverage (#170) and lintr (#176)

# xportr 0.3.0

## New Features and Bug Fixes

* Fixed an issue where `xportr_type()` would overwrite column labels, widths, and "sas.formats"
* Fixed messaging of `xportr_order()`to give better visibility of the number of variables being reordered.
* Add new argument to `xportr_write()` to allow users to specify how xpt validation checks are handled.
* Fixed bug where character_types were case sensitive. They are now case insensitive (#77).
* Updated `xportr_type()` to make type coercion more explicit.
* `xpt_validate` updated to accept iso8601 date formats. (#76)
* Added function `xportr_metadata()` to explicitly set metadata at the start of a pipeline (#44)
* Metadata order columns are now coerced to numeric by default in `xportr_order()` to prevent character sorting (#149)
* Message is shown on `xportr_*` functions when the metadata being used has multiple variables with the same name in the same domain (#128)
* Fixed an issue with `xport_type()` where `DT`, `DTM` variables with a format specified in the metadata (e.g. date9., datetime20.) were being converted to numeric, which will cause a 10 year difference when reading it back by `read_xpt()`. SAS's uniform start date is 1960 whereas Linux's uniform start date is 1970 (#142).
* Fixed an issue with R's pipe `|>` that was causing functions to abort (#97)
* Removed `<` and `>` as illegal characters in variable and dataset labels (#98)

## Documentation

* Moved `{pkgdown}` site to bootswatch. Enabled search and linked slack icon (#122).
* Additional Deep Dive vignette showcasing functions and quality of life utilities for processing `xpts` created (#84)
* Get Started vignette spruced up. Messages are now displayed and link to Deep Dive vignette (#150)
* Increase test coverage to 100% (#82)

## Deprecation and Breaking Changes

* The `metacore` argument has been renamed to `metadata` in the following six xportr functions: `xportr_df_label()`, `xportr_format()`, `xportr_label()`, `xportr_length()`, `xportr_order()`, and `xportr_type()`. Please update your code to use the new `metadata` argument in place of `metacore`.

# xportr 0.2.0

* Added a new validation test that errors when users pass invalid formats (#60 #64). Thanks to @zdz2101!
* Fixed an issue where xportr_format could pass invalid formats to haven::write_xpt.

# xportr 0.1.0

Beta release for xportr

* Added exported functions `xportr_varnames` and `xportr_tidy_rename` into `dev` folder found on GitHub Repostiory. Intention to move into packages after CRAN release.
* Fixed xportr_format() bug
* Using admiral ADSL dataset in examples

# xportr 0.0.0.9000

Initial alpha release of xportr

* Development of 5 core functions
* Package down site and documentation created

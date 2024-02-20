# xportr (development version)

## New Features and Bug Fixes

* `xportr_metadata()` can set `verbose` for a whole pipeline, i.e. setting `verbose` in `xportr_metadata()` will populate to all `xportr` functions.  (#151)
* All `xportr` functions now have `verbose = NULL` as the default (#151)
* Remove unused packages from Suggests (#221)

* `xportr_write()` now accepts `metadata` argument which can be used to set the dataset label to stay consistent with the other `xportr_*` functions. It is noteworthy that the dataset label set using the `xportr_df_label()` function will be retained during the `xportr_write()`.
* Exporting a new dataset `dataset_spec` that contains the Dataset Specification for ADSL. (#179)
* Added a check for character variable lengths up to 200 bytes in `xpt_validate()`(#91, #189).
* File name check is moved to strict_checks condition to allow underscores in the file name. Underscores are allowed in xpt but not per FDA requirements. (#126)

* It is now possible to get and set the xportr options using the helper function `xportr_options()` (#130)

* Added `xportr.character_metadata_types` and `xportr.numeric_metadata_types` to list the metadata types that are character or numeric. Updated `xportr.character_types` and `xportr.numeric_types` to list only the R types that are character and the R types that are numeric. This ensures that all R types, including dates, are now managed by xportr_type. If the R type differs from the metadata type, the variable is coerced (#161)..

* Adds argument assertions to public functions using `{checkmate}` (#175)

* `xportr_metadata()` can set `verbose` for a whole pipeline, i.e. setting `verbose` in `xportr_metadata()` will populate to all `xportr` functions.  (#151)

* All `xportr` functions now have `verbose = NULL` as the default. If left `NULL`, the previous default of `getOption("xportr.[fn_name]_verbose")` is used (#151)

* All core functions can be run together by using new function `xportr()` (#137)

* New argument in `xportr_length()` allows selection between the length from metadata, as previously done, or from the calculated maximum length per variable when `length_source` is set to “data” (#91)

## Deprecation and Breaking Changes

* The `domain` argument for xportr functions will no longer be dynamically 
determined by the name of the data frame passed as the .df argument. This was
done to make the use of xportr functions more explicit. (#182)

* The `label` argument from the `xportr_write()` function is deprecated in favor of the `metadata` argument. (#179)
* The `metacore` argument, which was renamed to `metadata` in the following six xportr functions: (`xportr_df_label()`, `xportr_format()`, `xportr_label()`, `xportr_length()`, `xportr_order()`, and `xportr_type()`) in version `0.3.0` with a soft deprecation warning, has now been hard deprecated. Please update your code to use the new `metadata` argument in place of `metacore`.

* `SASlength` and `SAStype` were removed since they did not have an impact on `xpt_validate` or any other functions (#132)
* Removes `admiral` from suggested dependencies (#278)

## Documentation

* Created development version of the website (#187)

* Additional guidance for options added in deep dive vignette (#81)

* Removed non-user facing function documentation (#192)

## Miscellaneous

* Tests use `{withr}` to create temporary files that are automatically deleted (#219)

# xportr 0.3.1

## New Features and Bug Fixes

* Make `xportr_type()` drop factor levels when coercing variables

## Documentation

* Set up Development version of Website (#187)

## Deprecation and Breaking Changes

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
* Fixed an issue with `xport_type()` where `DT`, `DTM` variables with a format specified in the metadata (e.g. `date9.`, `datetime20.`) were being converted to numeric, which will cause a 10 year difference when reading it back by `read_xpt()`. SAS's uniform start date is 1960 whereas Linux's uniform start date is 1970 (#142).
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

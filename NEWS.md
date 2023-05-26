# xportr 0.3.0

## New Features and Bug Fixes

* Fixed an issue where `xportr_type` would overwrite column labels, widths, and "sas.formats"
* Fixed messaging of `xportr_order`to give better visability of the number of variables being reordered.
* Add new argument to `xportr_write` to allow users to specify how xpt validation checks are handled.
* Fixed bug where character_types were case sensitive. They are now case insensitive.
* Updated `xportr_type` to make type coercion more explicit. 
* `xpt_validate` updated to accept iso8601 date formats.
* Added function `xportr_metadata()` to explicitly set metadata at the start of a pipeline (#44)

## Documentation

* Moved `{pkgdown}` site to bootswatch. Enabled search and linked slack icon (#122).

# xportr 0.2.0
* Added a new validation test that errors when users pass invalid formats (#60 #64). Thanks to @zdz2101!
* Fixed an issue where xportr_format could pass invalid formats to haven::write_xpt.

# xportr 0.1.0

Beta release for xportr 

* Added exported functions `xportr_varnames` and `xportr_tidy_rename` into `dev` folder found on GitHub Repostiory.  Intention to move into packages after CRAN release.
* Fixed xportr_format() bug
* Using admiral ADSL dataset in examples

# xportr 0.0.0.9000

Initial alpha release of xportr

* Development of 5 core functions
* Package down site and documentation created

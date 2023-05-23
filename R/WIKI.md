As xportr is an evolving package, functions or arguments may need to be removed or replaced with more efficient options from one release to another. In such cases, the relevant function or argument must be marked as deprecated. This deprecation is done in three phases over our release cycles.

Phase 1: The first release will issue a warning when using the depericated function or argument by using `lifecycle::deprecate_warn()`.
Phase 2: In the next release, an error will be thrown using `lifecycle::deprecate_stop()`.
Phase 3: Finally, in the 3rd release thereafter, the function will be removed from the package altogether.

## Examples of depreciation

### Deprecating a function

#### Phase 1

1. Use `lifecycle::deprecate_warn` inside the function to throw a deprecation watning when it is called. If the function is replaced by a newer function, provide the new function as the `with` argument inside the `lifecycle::deprecate_warn`
2. Add `lifecycle::badge("deprecated")` in the function description using roxygen comments
3. Build the document using `devtools::document()` to make sure that the vignettes are updated

```r
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is *deprecated*. Please use the function `xportr_new_func()` instead
xportr_old_func <- function((.df, metadata, domain = NULL, verbose = "none") {
    lifecycle::deprecate_warn(
        when = "1.0.0",
        what = "xportr_old_func()",
        with = "xportr_new_func()",
        details = "Optionally provide more context about this change"
    )
    # Function logic
}
```

#### Phase 2
1. Replace the `lifecycle::depricate_warn` with `lifecycle::deprecate_stop`
#### Phase 3
1. Completely remove the deprecated function from the package


### Deprecating a function argument

#### Phase 1

1. Still pass the deprecated argument in the function, but with a default value of `deprecated()`
2. If the deprecated function is used by the user, show a warning using `lifecycle::deprecate_warn`. If the argument is replaced with a new argument, reassign it here so the function works.
2. Add `lifecycle::badge("deprecated")` in the description of the function param using roxygen comments
3. Build the document using `devtools::document()` to make sure that the vignettes are updated

```r
#' @param old_arg `r lifecycle::badge("deprecated")` description about the argument
#' @param new_arg description about the new argument
#'
#' This function is *deprecated*. Please use the function `xportr_new_func()` instead
xportr_old_func <- function((.df, metadata, new_arg, domain = NULL, verbose = "none", old_arg = deprecated()) {
    if (!missing(old_arg)) {
        lifecycle::deprecate_warn(
            when = "1.0.0",
            what = "xportr_old_func(old_arg = )",
            with = "xportr_new_func(new_arg = )",
            details = "Optionally provide more context about this change"
        )
        nre_arg <- old_arg
    }
    # Function logic
}
```

#### Phase 2
1. Replace the `lifecycle::depricate_warn` with `lifecycle::deprecate_stop`
#### Phase 3
1. Completely remove the deprecated argument from the function


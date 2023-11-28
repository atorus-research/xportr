#' The `xportr` package
#'
#' `xportr` is designed to be a clinical workflow friendly method for outputting
#' CDISC complaint data sets in R, to XPT version 5 files. It was designed with
#' options in mind to allow for flexible setting of options while allowing
#' projects and system administrators to set sensible defaults for their
#' orginziations workflows. Below are a list of options that can be set to
#' customize how `xportr` works in your environment.
#'
#' @section xportr options:
#'
#' \itemize{
#'   \item{
#'   xportr.df_domain_name - The name of the domain "name" column in dataset
#'   metadata. Default: "dataset"
#'   }
#'   \item {
#'   xportr.df_label - The column noting the dataset label in dataset metadata.
#'   Default: "label"
#'   }
#'   \item{
#'   xportr.domain_name - The name of the domain "name" column in variable
#'   metadata. Default: "dataset"
#'   }
#'   \item{
#'   xportr.variable_name - The name of the variable "name" in variable
#'   metadata. Default: "variable"
#'   }
#'   \item{
#'   xportr.type_name - The name of the variable type column in variable
#'   metadata. Default: "type"
#'   }
#'   \item{
#'   xportr.label - The name of the variable label column in variable metadata.
#'   Default: "label"
#'   }
#'   \item{
#'   xportr.length - The name of the variable length column in variable
#'   metadata. Default: "length"
#'   }
#'   \item{
#'   xportr.format_name - The name of the variable format column in variable
#'   metadata. Default: "format"
#'   }
#'   \item{
#'   xportr.order_name - The name of the variable order column in variable
#'   metadata. Default: "order"
#'   }
#'   \item{
#'   xportr.format_verbose - The default argument for the 'verbose' argument for
#'   `xportr_format`. Default: "none"
#'   }
#'   \item{
#'   xportr.label_verbose - The default argument for the 'verbose' argument for
#'   `xportr_label`. Default: "none"
#'   }
#'   \item{
#'   xportr.length_verbose - The default argument for the 'verbose' argument for
#'   `xportr_length`. Default: "none"
#'   }
#'   \item{
#'   xportr.type_verbose - The default argument for the 'verbose' argument for
#'   `xportr_type`. Default: "none"
#'   }
#'   \item{
#'   xportr.character_types - The default character vector used to explicitly
#'   coerce R classes to character XPT types. Default: c("character", "char",
#'   "text", "date", "posixct", "posixt", "datetime", "time", "partialdate",
#'   "partialtime", "partialdatetime", "incompletedatetime", "durationdatetime",
#'   "intervaldatetime")
#'   }
#'   \item{
#'   xportr.numeric_types - The default character vector used to explicitly
#'   coerce R classes to numeric XPT types. Default: c("integer", "numeric",
#'   "num", "float")
#'   }
#' }
#'
#' @section Updating Options:
#' \itemize{
#'   \item{For a single session, an option can be changed by
#'   `option(<optionToChange> = <NewValue>)`}
#'   \item{To change an option for a single projects across sessions in that
#'   projects, place the options update in the `.Rprofile` in that project
#'   directory.}
#'   \item{To change an option for a user across all sessions, place the options
#'   update in the `.Rprofile` file in the users home directory.}
#'   \item{To change an option for all users in an R environment, place the
#'   options update in the `.Rprofile.site` file in the R home directory.}
#' }
#'
#'
#' @keywords internal
#'
#' @import rlang haven
#' @importFrom dplyr left_join bind_cols filter select rename rename_with n
#'   everything arrange group_by summarize mutate ungroup case_when distinct
#'   tribble if_else
#' @importFrom glue glue glue_collapse
#' @importFrom cli cli_alert_info cli_h2 cli_alert_success cli_div cli_text
#'   cli_alert_danger
#' @importFrom tidyselect all_of any_of
#' @importFrom utils capture.output str tail packageVersion
#' @importFrom stringr str_detect str_extract str_replace str_replace_all
#' @importFrom readr parse_number
#' @importFrom purrr map_chr map2_chr walk walk2 map map_dbl pluck
#' @importFrom janitor make_clean_names
#' @importFrom tm stemDocument
#' @importFrom graphics stem
#' @importFrom magrittr %>% extract2
#'
"_PACKAGE"

globalVariables(c(
  "abbr_parsed", "abbr_stem", "adj_orig", "adj_parsed", "col_pos", "dict_varname",
  "lower_original_varname", "my_minlength", "num_st_ind", "original_varname",
  "renamed_n", "renamed_var", "use_bundle", "viable_start", "type.x", "type.y",
  "variable"
))

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL

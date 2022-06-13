#' The `xportr` package
#'
#' Package Info here
#'
#' @keywords internal
#'
#' @import rlang haven
#' @importFrom purrr map_chr walk2 map map_dbl
#' @importFrom dplyr left_join bind_cols filter select rename rename_with n
#'   everything arrange group_by summarize mutate ungroup case_when distinct
#' @importFrom glue glue glue_collapse
#' @importFrom cli cli_alert_info cli_h2 cli_alert_success cli_alert_info
#'   cli_div cli_alert_success cli_text cli_h2
#' @importFrom tidyselect all_of any_of
#' @importFrom utils capture.output str tail packageVersion
#' @importFrom stringr str_detect str_extract str_replace str_replace_all
#' @importFrom readr parse_number
#' @importFrom purrr map_chr map2_chr
#' @importFrom janitor make_clean_names
#' @importFrom tm stemDocument
#' @importFrom graphics stem
#' @importFrom magrittr %>% extract2
#' 
"_PACKAGE"

globalVariables(c("abbr_parsed", "abbr_stem", "adj_orig", "adj_parsed", "col_pos", "dict_varname",
                  "lower_original_varname", "my_minlength", "num_st_ind", "original_varname",
                  "renamed_n", "renamed_var", "use_bundle", "viable_start"))

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

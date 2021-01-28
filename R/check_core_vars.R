# Title     : Checks SDTM variables for CDISC compliance.
# Objective : Add functions to check current state of SDTM CDISC variables.
#             Check next things:
#             - 'req' variable is not present or missing
#             - 'exp' variables are all blank
#             - 'perm' variables are all blank
#             - check the variable is CDISC variable
#             - 'cond' variables are all blank
#             - check that 'exp' and 'req' variable is not missing in dataset
# Created by: Rostyslav
# Created on: 1/20/2021


#' Three categories are specified in the "Core" column in the SDTM domain models:
#'
#' A Required variable is any variable that is basic to the identification of a data record (i.e., essential key
#' variables and a topic variable) or is necessary to make the record meaningful. Required variables must always be
#' included in the dataset and cannot be null for any record.
#'
#' An Expected variable is any variable necessary to make a record useful in the context of a specific domain. Expected
#' variables may contain some null values, but in most cases will not contain null values for every record. When the
#' study does not include the data item for an expected variable, however, a null column must still be included in the
#' dataset, and a comment must be included in the Define-XML document to state that the study does not include the
#' data item.
#'
#' A Permissible variable should be used in an SDTM dataset wherever appropriate. Although domain specification tables
#' list only some of the Identifier, Timing, and general observation class variables listed in the SDTM, all are
#' permissible unless specifically restricted in this implementation guide (see Section 2.7, SDTM Variables Not
#' Allowed in SDTMIG) or by specific domain assumptions.
#'     * Domain assumptions that say a Permissible variable is "generally not used" do not prohibit use of the variable.
#'     * If a study includes a data item that would be represented in a Permissible variable, then that variable must be
#'     included in the SDTM dataset, even if null. Indicate no data were available for that variable in the Define-XML
#'     document.
#'     * If a study did not include a data item that would be represented in a Permissible variable, then that variable
#'     should not be included in the SDTM dataset and should not be declared in the Define-XML document.
#'
#'
#' Conditionally required variables in ADaM (Cond) - must be included in the dataset in certain circumstances.
#'


library('readxl')
library('dplyr')
library('stringr')
library('SASxport')

# Before proceed to checking variables we need to load specification and make sure
# there is a column, named "Core" as well as "Variable" column.
#' Load spec from a file.
#'
#' By default 'Variables' tab is being read. To choose another one use 'sheet=' option.
#'
#' @details
#' @param spec. Path to a created specification or to a specification template.
#' @param sheet. Name (or sequential number) of the sheet to load.
#'
#' @return Tibble, containing dataset specifications.
#'
#' @example
#'
#' load_spec("ADaM_spec.xlsx", sheet = "Variables")
#'
load_spec <- function(spec., sheet. = "Variables"){
  s <- read_excel(spec., sheet=sheet.)

  # Check that spec contains "Variables" column.
  if ( !any(stringr::str_detect(string = names(s), stringr::regex("variable", TRUE))) ){
    stop("Column 'Variable' was not found in selected spec.")
  }

  # Check that spec contains "Core" column.
  if ( !any(stringr::str_detect(string = names(s), stringr::fixed("core", TRUE))) ){
    stop("Column 'Core' was not found in selected spec.")
  }

  return (s)
}


#' Calculate number of missing values withing a variable in dataset.
#'
#' @param dataset. Path do dataset, when var check is required.
#' @param variable. Variable to check for compliance.
#'
#' @return integer, number of NA occurances within specified variable.
#'
#' @example
#'
#' miss_count('adae.xpt', 'ASTDT')
#'
miss_count <- function(dataset., variable.){
  data <- read.xport(dataset.)

  if ( !any(stringr::str_detect(string = names(data), stringr::regex(variable., TRUE))) ){
    stop(paste0("Column ", variable., " was not found in dataset."))
  }

  return(
    sum(is.na(data[variable.]))
  )
}


#' Extract Variables and respecive categories.
#'
#' Construction brick for the following function as just does extraction of variables and respective
#' core category. To use the function spec hould be already filtered to contain only data related to particular
#' ADaM dataset.
#'
#' @param spec. A table-like object, containing actual SDTM or ADaM specification information.
#'
#' @return tibble, where column names are values from Variables column and values come from Core column.
#'
#' @examples
#'
#' core_vars <- load_spec("ADaM_spec.xlsx", sheet = "Variables") %>%
#'  get_core_vars_cat()
#'
get_core_vars_cat <- function(spec.){
  spec_vars <- select(.data = spec., Variable, Core)
  return(
    tidyr::pivot_wider(data = spec_vars, names_from = Variable, values_from = Core)
  )
}


#' Check variables according to their 'Core' category value.
#'
#' Do checks on CDISC compliance for each variable of the dataset, depending on its 'Core' category value. Values of
#' 'Core' category includes: Required, Expected, Permissible and Conditionally Required. Refer to https://www.cdisc.org/
#' for more details. If checks passed - will run silent, otherwise - throw errors or warnings.
#'
#' @param spec. A table-like object, containing actual SDTM or ADaM specification information.
#' @param dataset. Path do dataset, when var check is required.
#' @param ds_name. Optional; by default takes name of the dataset without extension; useful when dataset filename
#' differs from actual name of dataset (i.e. if ADAE file is named 'adae_final.xpt' etc.)
#' @param var_categ. Vector with names of 'Core' vategories to check; default: c("req", "exp", "perm", "cond").
#'
#' @return Nothing
#'
#' @examples
#' Consider having specifications or spec metadata in place ("ADaM_spec.xlsx"). Let ADAE be dataset we want to check.
#'
#' d <- load_spec("ADaM_spec.xlsx") %>%
#'  check_core("adae.xpt")
#'
#' If filename is diferent for any reason (like dataset had to be split to meet size expectations).
#'
#' d <- load_spec("analysis_metadata.xlsx") %>%
#'  check_core("mo1.xpt", ds_name = "MO")
#'
#' @export
#'
check_core <- function(spec., dataset., ds_name. = "", var_categ. = c("req", "exp", "perm", "cond")){
  # Assign dataset name to 'ds' or try to take from 'dataset.' param.
  if (missing(ds_name.)){
    ds <- tools::file_path_sans_ext(dataset.)
  }  else{
    ds <- ds_name.
  }

  # Keep only records, related to this dataset.
  spec. <- spec. %>% filter(stringr::str_detect(Dataset, stringr::fixed(ds, TRUE)))

  # Get list of Variables and their respective Core category.
  core_vars_w_cats <- get_core_vars_cat(spec.)

  # Obtain list of Variables actually present in the dataset.
  ds_vars_list <- names(read.xport(dataset.))

  # Calculate number of observations in dataset.
  obs <- nrow(read.xport(dataset.))

  # Obtain list of certain type variables ('req', 'exp', 'perm' - specified in function call in 'var_categ.'),
  # which should be present in the dataset.
  for (type in tolower(var_categ.)){
    vars_list <- core_vars_w_cats %>%
    select_if(function (col) stringr::str_detect(string = col, stringr::regex(type, TRUE))) %>%
      names()

    # Generate list of variables present in metadata and not in dataset.
    vars_not_in_dataset <- setdiff(vars_list, ds_vars_list)
    core_vars_in_data <- intersect(vars_list, ds_vars_list)

    # Generate list of variables with missing values.
    miss_list_all <- ""
    miss_list_any <- ""
    for (var in core_vars_in_data){
      if (miss_count(dataset., var)){
        miss_list_any <- append(miss_list_any, var)
      }
      if (miss_count(dataset., var) == obs){
        miss_list_all <- append(miss_list_all, var)
      }
    }
    miss_list_any <- miss_list_any[miss_list_any != ""]
    miss_list_all <- miss_list_all[miss_list_all != ""]

    # <--- Required vars ---> #
    if (type == 'req'){
      # If ANY 'req' variable is missing or has NA - stop.
      if (length(vars_not_in_dataset) > 0) {
        stop(paste0("Required variable(-s) ", vars_not_in_dataset, " are not present in ", dataset., "."))
      }
      if (length(miss_list_any) > 0){
        stop(paste0("Required variable(-s) ", paste0(miss_list_any, collapse = ' '), " in ", dataset., " contains missing values!
        Required variables must always be included in the dataset and cannot be null for any record. Please refer to
        https://www.cdisc.org/ for more details."))
      }
    }

    # <--- Expected vars ---> #
    if (type == 'exp'){
      # If ANY 'exp' variable is missing - stop.
      if (length(vars_not_in_dataset) > 0) {
        stop(paste0("Expected variable(-s) ", vars_not_in_dataset, " are not present in ", dataset., ". When the study
        does not include the data item for an expected variable, however, a null column must still be included in the
        dataset, and a comment must be included in the Define-XML document to state that the study does not include
        the data item. Please refer to https://www.cdisc.org/ for more details."))
      }
      # If ALL values of the expected variable are NA - put a warning.
      if (length(miss_list_all) > 0){
        warning(paste0("Expected variable(-s) ", paste0(miss_list_all, collapse = ' '), " in ", dataset., " has only NA values.
        Make sure to include comment in the Define-XML document to state that the study does not include the
        data item for the particular expected variable. Please refer to https://www.cdisc.org/ for more details."))
      }
    }

    # <--- Permissible vars ---> #
    if (type == 'perm'){
      # If ALL values of the permissible variable are NA - put a warning.
      if (length(miss_list_all) > 0){
        warning(paste0("Permissible variable(-s) ", paste0(miss_list_all, collapse = ' '), " in ", dataset., " has only NA values.
        Check if a study includes a data item that is represented in the particular permissible variable. If yes -
        make sure to include comment in the Define-XML document to indicate no data were available for that variable.
        Otherwise - remove the variable from dataset. Please refer to https://www.cdisc.org/ for more details."))
      }
    }

        # <--- Permissible vars ---> #
    if (type == 'cond'){
      # If ALL values of the conditionally required variable are NA - put a warning.
      if (length(miss_list_all) > 0){
        warning(paste0("Conditionally required variable(-s) ", paste0(miss_list_all, collapse = ' '), " in ", dataset., " has
        only NA values.
        Check if a study includes a data item that is represented in the particular variable. If yes -
        make sure to include comment in the Define-XML document to indicate no data were available for that variable.
        Otherwise - remove the variable from dataset. Please refer to https://www.cdisc.org/ for more details."))
      }
    }

  }

}

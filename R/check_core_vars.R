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


#' Load spec from a file.
#'
#' `load_spec()` returns a DataFrame with metadata, loaded from Excel file.
#'
#' @details
#' Before proceeding to checking variables we need to load specification and make sure
#' there is a column, named "Core" as well as "Variable" column.
#' By default 'Variables' tab is being read. To choose another one use 'sheet=' option.
#'
#' @param spec. Path to a created specification or to a specification template.
#' @param sheet. Name (or sequential number) of the sheet to load.
#'
#' @importFrom stringr str_detect
#' @importFrom stringr regex
#' @importFrom stringr fixed
#' @importFrom readxl read_excel
#'
#' @return Tibble, containing dataset specifications.
#' @noRd
#'
#' @examples
#' \dontrun{
#' load_spec("ADaM_spec.xlsx", sheet = "Variables")
#' }
load_spec <- function(spec., sheet. = "Variables"){
  s <- read_excel(file.path(spec.), sheet=sheet.)

  # Check that spec contains "Variables" column.
  if ( !any(str_detect(string = names(s), regex("variable", ignore_case = TRUE))) ){
    stop("Column 'Variable' was not found in selected spec.")
  }

  # Check that spec contains "Core" column.
  if ( !any(str_detect(string = names(s), fixed("core", ignore_case = TRUE))) ){
    stop("Column 'Core' was not found in selected spec.")
  }

  return (s)
}


#' Calculate number of missing values within a variable in dataset.
#'
#' @param dataset. Path to dataset, when var check is required.
#' @param variable. Variable to check for compliance.
#'
#' @return integer, number of NA occurances within specified variable.
#' @noRd
#'
#' @importFrom stringr str_detect
#' @importFrom stringr regex
#'
#' @examples
#' \dontrun{
#' miss_count(data_frame_object, 'ASTDT')
#' }
miss_count <- function(dataset., variable.){
  if ( !any(str_detect(string = names(dataset.), regex(variable., ignore_case = TRUE))) ){
    stop(paste0("Column ", variable., " was not found in dataset."))
  }

  return(
    sum(is.na(dataset.[variable.]))
  )
}


#' Extract Variables and respecive categories.
#'
#' Construction brick for the following function as just does extraction of variables and respective
#' core category. To use the function spec should be already filtered to contain only data related to particular
#' ADaM dataset.
#'
#' @param spec. A table-like object, containing actual SDTM or ADaM specification information.
#'
#' @return tibble, where column names are values from Variables column and values come from Core column.
#' @noRd
#'
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select
#'
#' @examples
#'
#' core_vars <- load_spec("ADaM_spec.xlsx", sheet = "Variables") %>%
#'  get_core_vars_cat()
#'
get_core_vars_cat <- function(spec.){

  spec_vars <- select(.data = spec., Variable, Core)
  return(
    pivot_wider(data = spec_vars, names_from = Variable, values_from = Core)
  )
}


#' Check variables according to their 'Core' category value.
#'
#' Do checks on CDISC compliance for each variable of the dataset, depending on its 'Core' category value. Values of
#' 'Core' category includes: Required, Expected, Permissible and Conditionally Required. Refer to https://www.cdisc.org/
#' for more details. If checks passed - will run silent, otherwise - throw errors or warnings.
#'
#' @param datadef A table-like object, containing actual SDTM or ADaM specification information.
#' @param .df Path do dataset, when var check is required.
#' @param ds_name. Optional; by default takes name of the dataset without extension; useful when dataset filename
#' differs from actual name of dataset (i.e. if ADAE file is named 'adae_final.xpt' etc.)
#' @param var_categ. Vector with names of 'Core' vategories to check; default: c("req", "exp", "perm", "cond").
#'
#' @importFrom stringr str_detect
#' @importFrom stringr regex
#' @importFrom stringr fixed
#' @importFrom tools file_path_sans_ext
#' @importFrom dplyr %>% select_if
#' @importFrom SASxport read.xport
#'
#' @return Nothing
#'
#' @examples
#' Consider having specifications or spec metadata in place ("ADaM_spec.xlsx"). Let ADAE be dataset we want to check.
#'
#' d <- load_spec("ADaM_spec.xlsx") %>%
#'  check_core("adae.xpt")
#'
#' If filename is different for any reason (like dataset had to be split to meet size expectations).
#'
#' d <- load_spec("analysis_metadata.xlsx") %>%
#'  check_core("mo1.xpt", ds_name = "MO")
#'
#' d <- load_spec("tests/testthat/files/ADaM_spec.xlsx") %>%
#'  check_core("adae.xpt", ds_name = "ADAE")
#'
#' Using 'datadef' tools:
#' dd <- define_to_DataDef(path_to_xml_file)
#' check_core(dd$ds_spec, "adae.xpt")
#'
#' @export
#'
check_core <- function(.df, datadef, ds_name. = "", var_categ. = c("req", "exp", "perm", "cond")){

  # Check if 'dataset.' is a a path-like string or a data frame object.
  if (is.character(.df)){
    dataset <- read.xport(.df)
  } else if (is.data.frame(.df)){
    dataset <- .df
    stopifnot("If 'dataset.' is not a path to file, specify dataset name in 'ds_name.'" = ds_name. != '')
  } else{
    stop("Parameter 'dataset.' should be a path to XPT or a data frame object.")
  }

  # Assign dataset name to 'ds' or try to take from 'dataset.' param.
  if (missing(ds_name.)){
    ds <- file_path_sans_ext(.df)
  } else {
    ds <- ds_name.
  }

  # Keep only records, related to this dataset.
  # A 'Dataset' is a column name of a spec. metadata.
  datadef <- datadef %>% dplyr::filter(str_detect(Dataset, fixed(ds, ignore_case = TRUE)))

  # Get list of Variables and their respective Core category.
  core_vars_w_cats <- get_core_vars_cat(datadef)

  # Obtain list of Variables actually present in the dataset.
  ds_vars_list <- names(dataset)

  # Calculate number of observations in dataset.
  obs <- nrow(dataset)

  # Obtain list of certain type variables ('req', 'exp', 'perm' - specified in function call in 'var_categ.'),
  # which should be present in the dataset.
  for (type in tolower(var_categ.)){
    vars_list <- core_vars_w_cats %>%
    dplyr::select_if(function (col) str_detect(col, regex(type, ignore_case = TRUE))) %>%
      names()

    # Generate list of variables present in metadata and not in dataset.
    vars_not_in_dataset <- setdiff(vars_list, ds_vars_list)
    core_vars_in_data <- intersect(vars_list, ds_vars_list)

    # Generate list of variables with missing values.
    miss_list_all <- ""
    miss_list_any <- ""
    for (var in core_vars_in_data){
      if (miss_count(dataset, var)){
        miss_list_any <- append(miss_list_any, var)
      }
      if (miss_count(dataset, var) == obs){
        miss_list_all <- append(miss_list_all, var)
      }
    }
    miss_list_any <- miss_list_any[miss_list_any != ""]
    miss_list_all <- miss_list_all[miss_list_all != ""]

    # <--- Required vars ---> #
    if (type == 'req'){
      # If ANY 'req' variable is missing or has NA - stop.
      if (length(vars_not_in_dataset) > 0) {
        stop(paste0("Required variable(-s) ", paste0(vars_not_in_dataset, collapse = ' '), " are not present in ", ds_name., "."))
      }
      if (length(miss_list_any) > 0){
        stop(paste0("Required variable(-s) ", paste0(miss_list_any, collapse = ' '), " in ", ds_name., " contains missing values!
        Required variables must always be included in the dataset and cannot be null for any record. Please refer to
        https://www.cdisc.org/ for more details."))
      }
    }

    # <--- Expected vars ---> #
    if (type == 'exp'){
      # If ANY 'exp' variable is missing - stop.
      if (length(vars_not_in_dataset) > 0) {
        stop(paste0("Expected variable(-s) ",  paste0(vars_not_in_dataset, collapse = ' '), " are not present in ", ds_name., ". When the study
        does not include the data item for an expected variable, however, a null column must still be included in the
        dataset, and a comment must be included in the Define-XML document to state that the study does not include
        the data item. Please refer to https://www.cdisc.org/ for more details."))
      }
      # If ALL values of the expected variable are NA - put a warning.
      if (length(miss_list_all) > 0){
        warning(paste0("Expected variable(-s) ", paste0(miss_list_all, collapse = ' '), " in ", ds_name., " has only NA values.
        Make sure to include comment in the Define-XML document to state that the study does not include the
        data item for the particular expected variable. Please refer to https://www.cdisc.org/ for more details."))
      }
    }

    # <--- Permissible vars ---> #
    if (type == 'perm'){
      # If ALL values of the permissible variable are NA - put a warning.
      if (length(miss_list_all) > 0){
        warning(paste0("Permissible variable(-s) ", paste0(miss_list_all, collapse = ' '), " in ", ds_name., " has only NA values.
        Check if a study includes a data item that is represented in the particular permissible variable. If yes -
        make sure to include comment in the Define-XML document to indicate no data were available for that variable.
        Otherwise - remove the variable from dataset. Please refer to https://www.cdisc.org/ for more details."))
      }
    }

        # <--- Conditionally required vars ---> #
    if (type == 'cond'){
      # If ALL values of the conditionally required variable are NA - put a warning.
      if (length(miss_list_all) > 0){
        warning(paste0("Conditionally required variable(-s) ", paste0(miss_list_all, collapse = ' '), " in ", ds_name., " has
        only NA values.
        Check if a study includes a data item that is represented in the particular variable. If yes -
        make sure to include comment in the Define-XML document to indicate no data were available for that variable.
        Otherwise - remove the variable from dataset. Please refer to https://www.cdisc.org/ for more details."))
      }
      else if (length(miss_list_any) > 0){
        warning(paste0("Conditionally required variable(-s) ", paste0(miss_list_any, collapse = ' '), " in ", ds_name., " contains missing values!
        Check if a study includes a data item that is represented in the particular variable. Please refer to
        https://www.cdisc.org/ for more details."))
      }
    }

  }

}

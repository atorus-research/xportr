# Title     : Checks SDTM variables for CDISC compliance.
# Objective : Add functions to check current state of SDTM CDISC variables.
#             Check next things:
#             - 'req' variable is not present or missing
#             - 'exp' variable is blank
#             - 'perm' variable is blank
#             - check the variable is CDISC variable
# Created by: Rostyslav
# Created on: 1/20/2021

#' From https://www.cdisc.org/standards/foundational/sdtmig/sdtmig-v3-3/html:
#' 4.1.5 SDTM Core Designations
#' Three categories are specified in the "Core" column in the domain models:
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


library('readxl')
library('testthat')
library('dplyr')
library('stringr')
library('SASxport')


# Before proceed to checking variables we need to load specification and make sure
# there is a column, named "Core" as well as "Variable" column.
#' Load spec from a file.
#' @details
#' @param spec Path to a created specification or to a specification template.
#' @param sheet Name (or sequential number) of the sheet to load.
#' @return Tibble, containing dataset specifications.
#' @example
#' loead_spec("ADaM_spec.xlsx", sheet = "Variables")
load_spec <- function(spec, sheet = "Variables"){
  s <- read_excel(spec, sheet=sheet)

  # Check that spec contains "Variables" column.
  expect_true(
    any(stringr::str_detect(string = names(s), stringr::regex("variable", TRUE))),
    label = "Column 'Variable' was not found in selected spec."
  )

  # Check that spec contains "Core" column.
  expect_true(
    any(stringr::str_detect(string = names(s), stringr::fixed("core", TRUE))),
    label = "Column 'Core' was not found in selected spec."
  )

  return (s)
}


#' Calculate number of missing values withing a variable in dataset.
#' @details
#' @param
#' @return integer, number of NA occurances within specified variable.
#' @example miss_count('adae.xpt', 'ASTDT')
miss_count <- function(dataset., variable.){
  data <- read.xport(dataset.)
  expect_true(
    any(stringr::str_detect(string = names(data), stringr::regex(variable., TRUE))),
    label = paste0("Column ", variable., " was not found in dataset.")
  )
  return(
    sum(is.na(data[variable.]))
  )
}


# Checkpoint #1 - Load spec.
# d <- load_spec(spec = "ADaM_spec.xlsx", sheet = "Variables")


#' Extract Variables and respecive categories.
#' @details Construction brick for the following function as just does extraction of variables and respective
#' core category. To use the function spec hould be already filtered to contain only data related to particular
#' ADaM dataset.
#' @param spec a table-like object, containing actual SDTM or ADaM specification information.
#' @return tibble, where column names are values from Variables column and values come from Core column.
#' @examples
#'
get_core_vars_cat <- function(spec){
  spec_vars <- select(.data = spec, Variable, Core)
  return(
    tidyr::pivot_wider(data = spec_vars, names_from = Variable, values_from = Core)
  )
}

# Checkpoint #2 - Extract vars and their categories.
# vars <- get_core_vars_cat(d)


#' Check if required variable from the spec is present in the data. Also check if non of the required variables contain
#' missing values.
#' @details
#' @param
#' @return
#' @examples
#'
check_req_vars <- function(spec., dataset., ds_name. = ""){
    # Assign dataset name to 'ds' or try to take from 'dataset.' param.
  if (missing(ds_name.)){
    # ds <- stringr::str_split(tools::file_path_sans_ext(dataset.), "_|-| |.")[[1]][1]
    ds <- toupper(tools::file_path_sans_ext(dataset.))
  }  else{
    ds <- toupper(ds_name.)
  }

  # Keep only records, related to this dataset.
  # TODO: why ignore case is not working? Just curious... done 'toupper' in the 'ds' assignment.
  spec. <- spec. %>% filter(Dataset == stringr::fixed(ds, TRUE))

  # Get list of Variables and their respective Core category.
  core_vars_w_cats <- get_core_vars_cat(spec.)

  # Obtain list of Required variables, which should be present in the dataset.
  req_vars_list <- core_vars_w_cats %>%
    select_if(function (col) stringr::str_detect(string = col, stringr::regex('req', TRUE))) %>%
    names()

  # Obtain list of Variables actually present in the dataset.
  ds_vars_list <- names(read.xport(dataset.))

  vars_not_in_dataset <- setdiff(req_vars_list, ds_vars_list)

  # Check for any 'req' variable not present in dataset.
  if (length(vars_not_in_dataset) > 0) {
    stop(paste0("Required variable(-s)", vars_not_in_dataset, " are not present in ", dataset., "."))
  }

  # Check for any 'req' variable has missing value.
  miss_list <- ""
  for (var in ds_vars_list){
    if (miss_count(dataset., var)){
      miss_list <- append(miss_list, var)
    }
  }

  if (length(miss_list) > 0){
    stop(paste0("Required variable ", miss_list, " in ", dataset., " contains missing values! It shouldn't per CDISC.
    Please refer to https://www.cdisc.org/ for more details."))
  }


  return (miss_list)
}

# Checkpoint #3 - Alarm when 'req' var is not present.
# check_req_vars_present(d, "adae.xpt")

#' @example
#'d <- load_spec("ADaM_spec.xlsx",
#'               "Variables") %>%
#'  check_req_vars_present("adae.xpt")
#'
#'d <- load_spec("ADaM_spec.xlsx",
#'               "Variables") %>%
#'  check_req_vars_present("adae_rd.xpt", "ADAE")

# Checkpoint #4 - Final Function 1 call.
d <- load_spec("ADaM_spec.xlsx") %>%
  check_req_vars("adae.xpt")



# TODO: function to check 'exp' vars for missings.
# Title     : Checks SDTM variables for CDISC compliance.
# Objective : Add functions to check current state of SDTM CDISC variables.
#             Check next things:
#             - 'req' variable is not present or missing
#             - 'exp' variable is blank
#             - 'perm' variable is blank
#             - check the variable is CDISC variable
# Created by: Rostyslav
# Created on: 1/20/2021

#' @details
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

# TODO:
# check_missing <- function ()
# check_empty <- function()

# Preliminary example of what I want to achieve.
# check <- function(type = ,         /* SDTM or ADaM */
#                   specdir = ,      /* a folder for programming specs. */
#                   datadir = ,      /* a folder for SDTM or ADaM datasets */
#                   domain = _ALL_   /*SDTM or ADaM domain name for checking */
#                   )

library('readxl')
library('testthat')
library('dplyr')
library('stringr')
library('SASxport')


# Before proceed to checking variables we need to load specification and make sure
# there is a column, named "Core".
#' Load spec from a file.
#' @details
#' If you do not pass ds_name parameter, try to guess name of dataset by the spec name.
#' @param spec Path to a created specification or to a specification template.
#' @param ds_name Name of the dataset.
#' @param sheet Name (or sequential number) of the sheet to load.
#' @return Tibble, containing dataset specifications.
#' @examples
#' loead_spec("ADaM_spec.xlsx", sheet = "Variables")
load_spec <- function(spec, ds_name, sheet = "Variables"){
  s <- read_excel(spec, sheet=sheet)

  # # TODO: Remove rows with NA.
  # s <- s[complete.cases(s), ]

  # Assign dataset name to 'ds' or try to take from spec name.
  if (missing(ds_name)){
    ds <- stringr::str_split(tools::file_path_sans_ext(spec), "_|-| ")[[1]][1]
  }  else{
    ds <- ds_name
  }

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

  s <- s %>% filter(Dataset == stringr::fixed(ds, TRUE))

  return (s)
}


# Checkpoint #1 - Load spec.
d <- load_spec(spec = "ADaM_spec.xlsx",
               ds_name = 'ADAE',
               sheet = "Variables")


#' Extract Variables and respecive categories.
#' @details
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
vars <- get_core_vars_cat(d)


#' Check if required variable from the spec is present in the data.
#' @details
#' @param
#' @return
#' @examples
#'
check_req_vars_present <- function(spec., dataset.){
  core_vars_w_cats <- get_core_vars_cat(spec.)

  # TODO: place that "guessing" of dataset name here and base on dataset. name.
  # TODO: add typing for the first parameter - "spec.". Let it be str as path to spec or could it be tibble.

  # Obtain list of Required variables, that should be present in the dataset.
  req_vars_list <- core_vars_w_cats %>%
    select_if(function (col) stringr::str_detect(string = col, stringr::regex('req', TRUE))) %>%
    names()

  # Obtain list of Variables actually present in the dataset.
  ds_vars_list <- names(read.xport(dataset.))

  vars_not_in_dataset <- setdiff(req_vars_list, ds_vars_list)

  #TODO: why this is not alarming?
  tryCatch(vars_not_in_dataset == 0, error = "Don't do this!")

  return ()
}

# Checkpoint #3 - Alarm when 'req' var is not present.
check_req_vars_present(d, "adae.xpt")
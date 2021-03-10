library(readxl)
library(dplyr)
library(haven)
library(cli)
library(purrr)


#' @title Extract Spec Variable Names for SDTM or ADAM Dataset
#'
#' @param ds A data frame of CDISC standard.
#' @param tab_model ADAM or SDTM choice to bring in appropriate Spec
#' @param vendor Declare which specs to use
#' @return Character vector of variable names from spec based on dataset
#'
spec_cols <- function(ds1, tab_model, vendor){
  
  #data_name <- deparse(substitute(ds1))
  
  if (vendor == "CDISC"){
    if (tab_model == "SDTM"){ 
      spec_ds <- read_xlsx("~/xptr/inst/specs/SDTM_spec.xlsx", sheet = 3) 
      ds_sub <- spec_ds[which(spec_ds$Dataset == ds1), ]
      ds_sub$Variable
      
    }
    else if (tab_model == "ADAM"){ 
      spec_ds <- read_xlsx("~/xptr/inst/specs/ADaM_spec.xlsx", sheet = 3) 
      ds_sub <- spec_ds[which(spec_ds$Dataset == ds1), ]
      ds_sub$Variable
    }
  }
  if (vendor == "GSK"){
    if (tab_model == "SDTM"){ 
      spec_ds <- read_xlsx("~/xptr/inst/specs/gsk_all_specs.xlsx", sheet = 3) 
      ds_sub <- spec_ds[which(spec_ds$Dataset == ds1), ]
      ds_sub$Variable
      
    }
    else if (tab_model == "ADAM"){ 
      spec_ds <- read_xlsx("~/xptr/inst/specs/gsk_all_specs.xlsx") 
      ds_sub <- spec_ds[which(spec_ds$`Domain Name` == ds1), ]
      ds_sub$`Variable Name`
    }
  }
}

# spec_cols("ADAE", tab_model = "ADAM", vendor = "GSK")

#' @title Extract Variable Names from input Dataset
#' 
#' @param .ds A data frame of CDISC standard.
#' @return Character vector of variables from spec based on dataset
#'
ds_cols <- function(ds2){
  ds2 %>% colnames()
}

# ds_cols(ADAE)

#' @title Search and identify variables that are in Spec and Dataset
#'
#' @param .ds A data frame of CDISC standard.
#' @param tab_model ADAM or SDTM choice to bring in appropriate Spec
#' @param verbose Boolean - Turns on messaging for what variables are in Spec and not in Spec
#' 
#' @return Variables that are in dataset and spec
xportr_spec_grab <- function(ds1, ds2, tab_model = tab_model, vendor = vendor, verbose){
  
  # Initialize vector for detected columns in dataset
  spec_grab <- c()
  
  for (i in ds_cols(ds2)){
    
    if (i %in% spec_cols(ds1,  tab_model = tab_model, vendor = vendor)){
      
      if (verbose == TRUE){

        cli_alert_success("Variable {i} found in Spec")

      } else {
        NULL
      }
      # Grab Variable Order from spec 
      
      spec_grab <- c(spec_grab, i)
      
      } else {
       NULL

      if (verbose == TRUE){
        cli_alert_danger("Variable {i} NOT found in Spec")

      } else {
        NULL
      }
    }
  }
  return(spec_grab)
}

# xportr_spec_grab("ADAE", ADAE, tab_model = "ADAM", vendor = "GSK", verbose = FALSE)

#' @title Order variables of a dataset according to Spec
#'
#' @param .ds A data frame of CDISC standard.
#' @param tab_model ADAM or SDTM choice to bring in appropriate Spec
#' @param vendor Specify vendor to access vendor specific spec file.
#' @param verbose Boolean - Turns on messaging for what variables are in Spec and not in Spec
#' @export
#' @return Dataframe that has been re-ordered according to spec
#' 
xportr_ord <- function(ds1, ds2, tab_model = tab_model, vendor = vendor, verbose) {
  
  #data_name2 <- deparse(substitute(.ds))
  
  cli_div(theme = list(span.emph = list(color = "orange")))
  cli_alert_success("I have retrieved the {.emph {vendor}} {.emph {tab_model}} Spec for you.")
  
  cli_h1("Starting to Order Variables according to Spec")
  cli_text("")
  
  # Grabs vars from Spec and inputted dataset
  vars_in_spec_ds <- xportr_spec_grab(
    ds1,
    ds2,
    vendor = vendor,
    tab_model = tab_model, 
    verbose = verbose)
  
  # Grabs all variables from Spec file and orders accordingly
  seq_vars <- ds2 %>% 
    select(all_of(vars_in_spec_ds)) 
  
  # Variables not in Spec file - will be moved to the end
  drop_vars <- ds2 %>% 
    select(!all_of(vars_in_spec_ds))
  
  # Used in warning message for how many vars have been moved
  moved_vars <- dim(drop_vars)[2]
  ordered_vars <- dim(seq_vars)[2]
  
  ds_seq <- bind_cols(seq_vars, drop_vars)
  
  if (moved_vars > 0) {
    cli_alert_info(c(
      "I have orderd {ordered_vars} variables according to {vendor} Spec and moved {moved_vars} variables that were not in the {vendor} Spec to the end of { } dataset"))
    
  } else if (moved_vars == 0){
    cli_alert_info(c(
      "I have orderd {ordered_vars} variables according to {vendor} {tab_model} Spec for { }"))
  }
  
  return(ds_seq)
}

xportr_ord("ADAE", ADAE, tab_model = "ADAM", vendor = "GSK", verbose = FALSE)

#' @title Apply Ordering to entire directory of datasets
#'
#' @param .dir path to folder with datasets
#' @param tab_model 
#' @param vendor 
#' @param verbose
#' @param data_type .sas7bdat, csv, xpt, ?
#' @param overwrite TRUE - overwrites the directory, FALSE - creates a new directory for datasets
#'
#' @return
#' @export
xportr_seq_dir <- function(tab_model, vendor, verbose, data_type, overwrite){
  
  # cdisc_data <- list.files(path = "~/xptr/inst/extdata", pattern = ".sas7bdat")
  # 
  # cdisc_data_2 <- paste0("~/xptr/inst/extdata/", cdisc_data)
  # 
  # dir.create("~/xptr/inst/extdata/ordered_data")
  # 
  # cdisc_data_3 <- lapply(cdisc_data_2, read_sas)
  # 
  # #lapply(cdisc_data_3, xportr_seq(tab_model = "ADAM", vendor = "GSK", verbose = FALSE))
  # 
  # for (i in cdisc_data_3){
  #   
  #   xportr_seq(i, tab_model = "ADAM", vendor = "GSK", verbose = FALSE)
  # }
  # 
  # 
  # # if file is .sas7bdat then read in 
  # # Create list of data_frames
  # 
  # # cdisc_data <- list(ADAE, ADSL, ADLB)
  # # 
  # # for (i in cdisc_data){
  # #   xportr_seq(i, tab_model = "ADAM", vendor = "GSK", verbose = FALSE)
  # # }
  # # 
}

#xportr_seq_dir()

#demo_spinners(sample(list_spinners(), 10))

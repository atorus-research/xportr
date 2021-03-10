library(readxl)
library(dplyr)
library(haven)
library(cli)
library(purrr)
library(stringr)


#' @title Extract Spec Variable Names for SDTM or ADAM Dataset
#'
#' @param df1 df1 A character input to subset Specs with
#' @param tab_model ADAM or SDTM choice to bring in appropriate Spec
#' @param vendor Declare which specs to use
#' @return Character vector of variable names from spec based on dataset
#'
get_spec_col_names <- function(df1, tab_model, vendor){
  

  if (vendor == "CDISC"){
    if (tab_model == "SDTM"){ 
      spec_ds <- read_xlsx("~/xptr/inst/specs/SDTM_spec.xlsx", sheet = 3) 
      ds_sub <- spec_ds[which(spec_ds$Dataset == df1), ]
      ds_sub$Variable
      
    }
    else if (tab_model == "ADAM"){ 
      spec_ds <- read_xlsx("~/xptr/inst/specs/ADaM_spec.xlsx", sheet = 3) 
      ds_sub <- spec_ds[which(spec_ds$Dataset == df1), ]
      ds_sub$Variable
    }
  }
  if (vendor == "GSK"){
    if (tab_model == "SDTM"){ 
      spec_ds <- read_xlsx("~/xptr/inst/specs/gsk_all_specs.xlsx", sheet = 3) 
      ds_sub <- spec_ds[which(spec_ds$Dataset == df1), ]
      ds_sub$Variable
      
    }
    else if (tab_model == "ADAM"){ 
      spec_ds <- read_xlsx("~/xptr/inst/specs/gsk_all_specs.xlsx") 
      ds_sub <- spec_ds[which(spec_ds$`Domain Name` == df1), ]
      ds_sub$`Variable Name`
    }
  }
}

#' @title Extract Variable Names from input Dataset
#' 
#' @param df2 A data frame of CDISC standard.
#' @return Character vector of variables from spec based on dataset
#'
get_df_col_names <- function(df2){
  df2 %>% colnames()
}

#' @title Search and identify variables that are in Spec and Dataset
#'
#' @param df1 A character input to subset Specs with
#' @param df2 A data frame of CDISC standard.
#' @param tab_model ADAM or SDTM choice to bring in appropriate Spec
#' @param verbose Boolean - Turns on messaging for what variables are in Spec and not in Spec
#' 
#' @return Variables that are in dataset and spec
xportr_spec_grab <- function(df1, df2, tab_model = tab_model, vendor = vendor, verbose){
  
  # Initialize vector for detected columns in dataset
  spec_grab <- c()
  
  for (i in get_df_col_names(df2)){
    if (i %in% get_spec_col_names(df1,  tab_model = tab_model, vendor = vendor)){
      if (verbose == TRUE){
        cli_alert_success("Variable {i} found in {df1} Spec")

      } else {
        NULL
      }
      # Grab Variable Order from spec 
      spec_grab <- c(spec_grab, i)
      
      } else {
       NULL

      if (verbose == TRUE){
        cli_alert_danger("Variable {i} NOT found in {df1} Spec")
      } else {
        NULL
      }
    }
  }
  return(spec_grab)
}

#' @title Order variables of a dataset according to Spec
#'
#' @param df1 A character input to subset Specs with
#' @param df2 A data frame of CDISC standard.
#' @param tab_model ADAM or SDTM choice to bring in appropriate Spec
#' @param vendor Specify vendor to access vendor specific spec file.
#' @param verbose Boolean - Turns on messaging for what variables are in Spec and not in Spec
#' @export
#' @return Dataframe that has been re-ordered according to spec
#' 
xportr_ord <- function(df1, df2, tab_model = tab_model, vendor = vendor, verbose) {
  
  #data_name2 <- deparse(substitute(.ds))
  
  cli_div(theme = list(span.emph = list(color = "orange")))
  cli_alert_success("I have retrieved the {.emph {vendor}} {.emph {tab_model}} Spec for you.")
  
  cli_h1("Starting to Order Variables according to Spec")
  cli_text("")
  
  # Grabs vars from Spec and inputted dataset
  vars_in_spec_ds <- xportr_spec_grab(
    df1,
    df2,
    vendor = vendor,
    tab_model = tab_model, 
    verbose = verbose)
  
  # Grabs all variables from Spec file and orders accordingly
  ord_vars <- df2 %>% 
    select(all_of(vars_in_spec_ds)) 
  
  # Variables not in Spec file - will be moved to the end
  drop_vars <- df2 %>% 
    select(!all_of(vars_in_spec_ds))
  
  # Used in warning message for how many vars have been moved
  moved_vars <- dim(drop_vars)[2]
  ordered_vars <- dim(ord_vars)[2]
  
  df_re_ord <- bind_cols(ord_vars, drop_vars)
  
  if (moved_vars > 0) {
    cli_alert_info(c(
      "I have orderd {ordered_vars} variables according to {vendor} {df1} Spec and moved {moved_vars} variables that were not in the {vendor} {df1} Spec to the end of {df1} dataset"))
    
  } else if (moved_vars == 0){
    cli_alert_info(c(
      "I have orderd {ordered_vars} variables according to {vendor} {tab_model} {df1} Spec for {df1}"))
  }
  
  return(df_re_ord)
}

#' @title Apply Ordering to entire directory of datasets
#'
#' @param path path to folder with datasets
#' @param tab_model 
#' @param vendor 
#' @param verbose
#' @param data_type .sas7bdat, csv, xpt, ?
#' @param overwrite TRUE - overwrites the directory, FALSE - creates a new directory for datasets
#'
#' @return
#' @export
xportr_ord_dir <- function(path, tab_model, vendor, verbose){
  
  # Supply path to directort
  cdisc_data <- list.files(path = path, pattern = ".sas7bdat")
  
  cdisc_name_strip <- str_to_upper(str_remove(cdisc_data, ".sas7bdat"))
   
  cdisc_data_2 <- paste0("~/xptr/inst/extdata/", cdisc_data)
  
  cdisc_data_3 <- lapply(cdisc_data_2, read_sas)
  
  for (i in 1:length(cdisc_data_2)) {
    assign(paste0(cdisc_name_strip[i]), cdisc_data_3[[i]])
  }
  
  for (i in cdisc_name_strip){
      
        xportr_ord(df1 = as.character(cdisc_name_strip[i]), 
                   df2 = cdisc_data_3[[i]],
                   tab_model = tab_model, vendor = vendor, verbose = FALSE)
  }
}
   
xportr_ord_dir(path = "~/xptr/inst/extdata/", tab_model = "ADAM", vendor = "GSK", verbose = FALSE)







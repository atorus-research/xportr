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
#' @param path_to_spec USer supplies location to Spec
#' @param vendor Declare which specs to use
#' @return Character vector of variable names from spec based on dataset
#'

get_spec_col_names <- function(df1, tab_model, path_to_spec, vendor){
  
  tab_vendor <- switch(
    paste(tab_model, vendor, sep = " "),
    "SDTM CDISC" = {spec_ds <- read_xlsx(paste0(path_to_spec, "/SDTM_spec.xlsx"), sheet = 3)
                    ds_sub <- spec_ds[which(spec_ds$Dataset == df1), ]
                    ds_sub$Variable},
    
    "ADAM CDISC" = {spec_ds <- read_xlsx(paste0(path_to_spec, "/ADaM_spec.xlsx"), sheet = 3)
                    ds_sub <- spec_ds[which(spec_ds$Dataset == df1), ]
                    ds_sub$Variable},
    
    "SDTM GSK"   =  {spec_ds <- read_xlsx(paste0(path_to_spec, "/gsk_all_specs.xlsx"), sheet = 1)
                    ds_sub <- spec_ds[which(spec_ds$Dataset == df1), ]
                    ds_sub$Variable},
    
    "ADAM GSK"   = {spec_ds <- read_xlsx(paste0(path_to_spec, "/gsk_all_specs.xlsx"), sheet = 1)
                    ds_sub <- spec_ds[which(spec_ds$`Domain Name` == df1), ]
                    ds_sub$`Variable Name`}
  )
}


#' @title Extract Variable Names from input dataset
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
xportr_spec_grab <- function(df1, df2, 
                             tab_model = tab_model, 
                             path_to_spec = path_to_spec, 
                             vendor = vendor, verbose){
  
  # Initialize vector for detected columns in dataset
  spec_grab <- c()
  
  for (i in get_df_col_names(df2)){
    if (i %in% get_spec_col_names(df1,  tab_model = tab_model, path_to_spec = path_to_spec, vendor = vendor)){
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
#' @param msg_var_order Boolean - Turns on more messaging for inputs beings used for ordering.  Also tells you how many
#' variables have been ordered and how many have been moved to the end of the dataset. 
#' @export
#' @return Dataframe that has been re-ordered according to spec
#' 
xportr_ord <- function(df1, df2, 
                       tab_model = tab_model, 
                       path_to_spec = path_to_spec, 
                       vendor = vendor, 
                       verbose,
                       msg_var_order) {
  
  if (msg_var_order == TRUE) {
    
    # Function is located in messages.R
    var_order_msg_alert(df1, vendor, tab_model)  
  
  }
  
  # Grabs vars from Spec and inputted dataset
  vars_in_spec_ds <- xportr_spec_grab(
    df1, df2, tab_model = tab_model, path_to_spec = path_to_spec, vendor = vendor, verbose)
  
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
  
  if (msg_var_order == TRUE) {
    
    # Function is located in messages.R
    var_ord_msg_success(df1, ordered_vars, moved_vars, vendor, tab_mode)  
    
  }

  return(df_re_ord)
}







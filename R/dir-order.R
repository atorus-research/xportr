#################
# This is not working - happy to hear ideas!
#################

#' @title Apply Ordering of Spec to entire directory of datasets
#'
#' @param path path to folder with datasets
#' @param pattern Type of file for function to identify and reorder - sas7bdat, csv, xpt, Rdata, others?
#' @param tab_model 
#' @param vendor 
#' @param verbose
#' @param data_type .sas7bdat, csv, xpt, ?
#' @param overwrite TRUE - overwrites the directory, FALSE - creates a new directory for datasets
#'
#' @return
#' @export
xportr_ord_dir <- function(path, pattern, tab_model = tab_model, vendor = vendor, verbose = verbose){
  
  # Supply path to directort
  cdisc_data <- list.files(path = path, pattern = pattern)
  
  cdisc_name_strip <- str_to_upper(str_remove(cdisc_data, pattern))
  
  cdisc_name_lst <- as.list(cdisc_name_strip)
  
  cdisc_data_2 <- paste0("~/xptr/inst/extdata/", cdisc_data)
  
  cdisc_data_3 <- lapply(cdisc_data_2, read_sas)
  
  names(cdisc_data_3) <- cdisc_name_strip
  
  for (i in 1:length(cdisc_data_2)) {
    assign(paste0(cdisc_name_strip[i]), cdisc_data_3[[i]])
  }
  
  for (i in cdisc_name_strip){
    for (i in 1:length(cdisc_name_strip)){
      
      print(cdisc_name_strip[i])
      print(as.data.frame(cdisc_data_3[i]))
      
      xportr_ord(df1 = cdisc_name_lst[i],
                 df2 = as.data.frame(cdisc_data_3[i]),
                 tab_model = tab_model, vendor = vendor, verbose = verbose)
    }
  }
}

# xportr_ord_dir(path = "~/xptr/inst/extdata/",
#                pattern = ".sas7bdat",
#                tab_model = "ADAM",
#                vendor = "GSK",
#                verbose = T)



# Old Code from xportr_ord function for tab and vendor inputs - updated with swtich statement
# if (vendor == "CDISC"){
#   if (tab_model == "SDTM"){ 
#     spec_ds <- read_xlsx(paste0(path_to_spec, "/SDTM_spec.xlsx"), sheet = 3)
#     #spec_ds <- read_xlsx("~/xptr/inst/specs/SDTM_spec.xlsx", sheet = 3) 
#     ds_sub <- spec_ds[which(spec_ds$Dataset == df1), ]
#     ds_sub$Variable
#     
#   }
#   else if (tab_model == "ADAM"){ 
#     spec_ds <- read_xlsx(paste0(path_to_spec, "/ADaM_spec.xlsx"), sheet = 3)
#     #spec_ds <- read_xlsx("~/xptr/inst/specs/ADaM_spec.xlsx", sheet = 3) 
#     ds_sub <- spec_ds[which(spec_ds$Dataset == df1), ]
#     ds_sub$Variable
#   }
# }
# if (vendor == "GSK"){
#   if (tab_model == "SDTM"){
#     spec_ds <- read_xlsx(paste0(path_to_spec, "/gsk_all_specs.xlsx"), sheet = 1)
#     #spec_ds <- read_xlsx("~/xptr/inst/specs/gsk_all_specs.xlsx", sheet = 3) 
#     ds_sub <- spec_ds[which(spec_ds$Dataset == df1), ]
#     ds_sub$Variable
#     
#   }
#   else if (tab_model == "ADAM"){ 
#     spec_ds <- read_xlsx(paste0(path_to_spec, "/gsk_all_specs.xlsx"), sheet = 1)
#     #spec_ds <- read_xlsx("~/xptr/inst/specs/gsk_all_specs.xlsx") 
#     ds_sub <- spec_ds[which(spec_ds$`Domain Name` == df1), ]
#     ds_sub$`Variable Name`
#   }
# }
#}











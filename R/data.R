#' Analysis Dataset Subject Level
#'
#' An example dataset containing subject level data
#'
#' @source Dataset created by `admiral::use_ad_template("adsl")`
#' @usage data("adsl_xportr")
#'
#' @format ## `adsl_xportr`
#' A data frame with 306 rows and 51 columns:
#' \describe{
#'   \item{STUDYID}{Study Identifier}
#'   \item{USUBJID}{Unique Subject Identifier}
#'   \item{SUBJID}{Subject Identifier for the Study}
#'   \item{RFSTDTC}{Subject Reference Start Date/Time}
#'   \item{RFENDTC}{Subject Reference End Date/Time}
#'   \item{RFXSTDTC}{Date/Time of First Study Treatment}
#'   \item{RFXENDTC}{Date/Time of Last Study Treatment}
#'   \item{RFICDTC}{Date/Time of Informed Consent}
#'   \item{RFPENDTC}{Date/Time of End of Participation}
#'   \item{DTHDTC}{Date/Time of Death}
#'   \item{DTHFL}{Subject Death Flag}
#'   \item{SITEID}{Study Site Identifier}
#'   \item{AGE}{Age}
#'   \item{AGEU}{Age Units}
#'   \item{SEX}{Sex}
#'   \item{RACE}{Race}
#'   \item{ETHNIC}{Ethnicity}
#'   \item{ARMCD}{Planned Arm Code}
#'   \item{ARM}{Description of Planned Arm}
#'   \item{ACTARMCD}{Actual Arm Code}
#'   \item{ACTARM}{Description of Actual Arm}
#'   \item{COUNTRY}{Country}
#'   \item{DMDTC}{Date/Time of Collection}
#'   \item{DMDY}{Study Day of Collection}
#'   \item{TRT01P}{Planned Treatment for Period 01}
#'   \item{TRT01A}{Actual Treatment for Period 01}
#'   \item{TRTSDTM}{Datetime of First Exposure to Treatment}
#'   \item{TRTSTMF}{Time of First Exposure Imputation Flag}
#'   \item{TRTEDTM}{Datetime of Last Exposure to Treatment}
#'   \item{TRTETMF}{Time of Last Exposure Imputation Flag}
#'   \item{TRTSDT}{Date of First Exposure to Treatment}
#'   \item{TRTEDT}{Date of Last Exposure to Treatment}
#'   \item{TRTDURD}{Total Treatment Duration (Days)}
#'   \item{SCRFDT}{Screen Failure Date}
#'   \item{EOSDT}{End of Study Date}
#'   \item{EOSSTT}{End of Study Status}
#'   \item{FRVDT}{Final Retrieval Visit Date}
#'   \item{RANDDT}{Date of Randomization}
#'   \item{DTHDT}{Date of Death}
#'   \item{DTHDTF}{Date of Death Imputation Flag}
#'   \item{DTHADY}{Relative Day of Death}
#'   \item{LDDTHELD}{Elapsed Days from Last Dose to Death}
#'   \item{LSTALVDT}{Date Last Known Alive}
#'   \item{SAFFL}{Safety Population Flag}
#'   \item{RACEGR1}{Pooled Race Group 1}
#'   \item{AGEGR1}{Pooled Age Group 1}
#'   \item{REGION1}{Geographic Region 1}
#'   \item{LDDTHGR1}{Last Dose to Death - Days Elapsed Group 1}
#'   \item{DTH30FL}{Death Within 30 Days of Last Trt Flag}
#'   \item{DTHA30FL}{Death After 30 Days from Last Trt Flag}
#'   \item{DTHB30FL}{Death Within 30 Days of First Trt Flag}
#' }
"adsl_xportr"

#' Example Dataset Variable Specification
#'
#' @usage data("var_spec")
#'
#' @format ## `var_spec`
#' A data frame with 216 rows and 19 columns:
#' \describe{
#'   \item{Order}{Order of variable}
#'   \item{Dataset}{Dataset}
#'   \item{Variable}{Variable}
#'   \item{Label}{Variable Label}
#'   \item{Data Type}{Data Type}
#'   \item{Length}{Variable Length}
#'   \item{Significant Digits}{Significant Digits}
#'   \item{Format}{Variable Format}
#'   \item{Mandatory}{Mandatory Variable Flag}
#'   \item{Assigned Value}{Variable Assigned Value}
#'   \item{Codelist}{Variable Codelist}
#'   \item{Common}{Common Variable Flag}
#'   \item{Origin}{Variable Origin}
#'   \item{Pages}{Pages}
#'   \item{Method}{Variable Method}
#'   \item{Predecessor}{Variable Predecessor}
#'   \item{Role}{Variable Role}
#'   \item{Comment}{Comment}
#'   \item{Developer Notes}{Developer Notes}
#' }
"var_spec"

#' Example Dataset Specification
#'
#' @usage data("dataset_spec")
#' @format ## `dataset_spec`
#' A data frame with 1 row and 9 columns:
#' \describe{
#'   \item{Dataset}{chr: Dataset}
#'   \item{Description}{chr: Dataset description}
#'   \item{Class}{chr: Dataset class}
#'   \item{Structure}{lgl: Logical, indicating if there's a specific structure}
#'   \item{Purpose}{chr: Purpose of the dataset}
#'   \item{Key, Variables}{chr: Join Key variables in the dataset}
#'   \item{Repeating}{chr: Indicates if the dataset is repeating}
#'   \item{Reference Data}{lgl: Reference Data}
#'   \item{Comment}{chr: Additional comment}
#' }
"dataset_spec"

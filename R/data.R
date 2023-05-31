#' Analysis Dataset Subject Level
#'
#' An example dataset containing subject level data
#'
#' @format ## `adsl`
#' A data frame with 254 rows and 48 columns:
#' \describe{
#'   \item{STUDYID}{Study Identifier}
#'   \item{USUBJID}{Unique Subject Identifier}
#'   \item{SUBJID}{Subject Identifier for the Study}
#'   \item{SITEID}{Study Site Identifier}
#'   \item{SITEGR1}{Pooled Site Group 1}
#'   \item{ARM}{Description of Planned Arm}
#'   \item{TRT01P}{Planned Treatment for Period 01}
#'   \item{TRT01PN}{Planned Treatment for Period 01 (N)}
#'   \item{TRT01A}{Actual Treatment for Period 01}
#'   \item{TRT01AN}{Actual Treatment for Period 01 (N)}
#'   \item{TRTSDT}{Date of First Exposure to Treatment}
#'   \item{TRTEDT}{Date of Last Exposure to Treatment}
#'   \item{TRTDUR}{Duration of Treatment (days)}
#'   \item{AVGDD}{Avg Daily Dose (as planned)}
#'   \item{CUMDOSE}{Cumulative Dose (as planned)}
#'   \item{AGE}{Age}
#'   \item{AGEGR1}{Pooled Age Group 1}
#'   \item{AGEGR1N}{Pooled Age Group 1 (N)}
#'   \item{AGEU}{Age Units}
#'   \item{RACE}{Race}
#'   \item{RACEN}{Race (N)}
#'   \item{SEX}{Sex}
#'   \item{ETHNIC}{Ethnicity}
#'   \item{SAFFL}{Safety Population Flag}
#'   \item{ITTFL}{Intent-To-Treat Population Flag}
#'   \item{EFFFL}{Efficacy Population Flag}
#'   \item{COMP8FL}{Completers of Week 8 Population Flag}
#'   \item{COMP16FL}{Completers of Week 16 Population Flag}
#'   \item{COMP24FL}{Completers of Week 24 Population Flag}
#'   \item{DISCONFL}{Did the Subject Discontinue the Study}
#'   \item{DSRAEFL}{Discontinued due to AE}
#'   \item{DTHFL}{Subject Died}
#'   \item{BMIBL}{Baseline BMI (kg/m^2)}
#'   \item{BMIBLGR1}{Pooled Baseline BMI Group 1}
#'   \item{HEIGHTBL}{Baseline Height (cm)}
#'   \item{WEIGHTBL}{Baseline Weight (kg)}
#'   \item{EDUCLVL}{Years of Education}
#'   \item{DISONSDT}{Date of Onset of Disease}
#'   \item{DURDIS}{Duration of Disease (Months)}
#'   \item{DURDSGR1}{Pooled Disease Duration Group 1}
#'   \item{VISIT1DT}{Date of Visit 1}
#'   \item{RFSTDTC}{Subject Reference Start Date/Time}
#'   \item{RFENDTC}{Subject Reference End Date/Time}
#'   \item{VISNUMEN}{End of Trt Visit (Vis 12 or Early Term.)}
#'   \item{RFENDT}{Date of Discontinuation/Completion}
#'   \item{DCDECOD}{Standardized Disposition Term}
#'   \item{DCREASCD}{Reason for Discontinuation}
#'   \item{MMSETOT}{MMSE Total}
#' }
"adsl"

#' Example Dataset Specification
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

# Analysis Dataset Subject Level

An example dataset containing subject level data

## Usage

``` r
data("adsl_xportr")
```

## Format

### `adsl_xportr`

A data frame with 306 rows and 51 columns:

- STUDYID:

  Study Identifier

- USUBJID:

  Unique Subject Identifier

- SUBJID:

  Subject Identifier for the Study

- RFSTDTC:

  Subject Reference Start Date/Time

- RFENDTC:

  Subject Reference End Date/Time

- RFXSTDTC:

  Date/Time of First Study Treatment

- RFXENDTC:

  Date/Time of Last Study Treatment

- RFICDTC:

  Date/Time of Informed Consent

- RFPENDTC:

  Date/Time of End of Participation

- DTHDTC:

  Date/Time of Death

- DTHFL:

  Subject Death Flag

- SITEID:

  Study Site Identifier

- AGE:

  Age

- AGEU:

  Age Units

- SEX:

  Sex

- RACE:

  Race

- ETHNIC:

  Ethnicity

- ARMCD:

  Planned Arm Code

- ARM:

  Description of Planned Arm

- ACTARMCD:

  Actual Arm Code

- ACTARM:

  Description of Actual Arm

- COUNTRY:

  Country

- DMDTC:

  Date/Time of Collection

- DMDY:

  Study Day of Collection

- TRT01P:

  Planned Treatment for Period 01

- TRT01A:

  Actual Treatment for Period 01

- TRTSDTM:

  Datetime of First Exposure to Treatment

- TRTSTMF:

  Time of First Exposure Imputation Flag

- TRTEDTM:

  Datetime of Last Exposure to Treatment

- TRTETMF:

  Time of Last Exposure Imputation Flag

- TRTSDT:

  Date of First Exposure to Treatment

- TRTEDT:

  Date of Last Exposure to Treatment

- TRTDURD:

  Total Treatment Duration (Days)

- SCRFDT:

  Screen Failure Date

- EOSDT:

  End of Study Date

- EOSSTT:

  End of Study Status

- FRVDT:

  Final Retrieval Visit Date

- RANDDT:

  Date of Randomization

- DTHDT:

  Date of Death

- DTHDTF:

  Date of Death Imputation Flag

- DTHADY:

  Relative Day of Death

- LDDTHELD:

  Elapsed Days from Last Dose to Death

- LSTALVDT:

  Date Last Known Alive

- SAFFL:

  Safety Population Flag

- RACEGR1:

  Pooled Race Group 1

- AGEGR1:

  Pooled Age Group 1

- REGION1:

  Geographic Region 1

- LDDTHGR1:

  Last Dose to Death - Days Elapsed Group 1

- DTH30FL:

  Death Within 30 Days of Last Trt Flag

- DTHA30FL:

  Death After 30 Days from Last Trt Flag

- DTHB30FL:

  Death Within 30 Days of First Trt Flag

## Source

Dataset created by `admiral::use_ad_template("adsl")`

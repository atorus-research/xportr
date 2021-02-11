
# Create dummy datasets.
t_adae <- data.frame(USUBJID=c('U0001', 'U0001', 'U0002', 'U0002', 'U0003'),  # req OK
                     AEDECOD=c('Heart Failure', NA, 'Sinusitis', 'Headache', 'Angioedema'),  # req error
                     AESOCCD=c(NA, NA, NA, NA, NA),  # perm warn
                     APERIOD=c(NA, NA, 1, 2, 1),  # perm OK
                     TRTEMFL=c('Y', 'Y', 'N', 'Y', 'Y'),  # cond OK
                     AESOC=c('Cardiovascular', NA, NA, NA, NA) # cond warn
)
t_ds <- data.frame(USUBJID=c('U0001', 'U0001', 'U0002', 'U0002', 'U0003'),  # req OK
                   DSCAT=c(NA, NA, NA, NA, NA),  # exp warn
                   VISIT=c(NA, NA, NA, NA, NA),  # perm warn
                   DSDY=c(NA, NA, 1, 21, 1)  # exp warn
)
t_tv <- data.frame(STUDYID='S0010001',
                   DOMAIN='TV',
                   VISITNUM=1,
                   VISIT='Visit 1',
                   VISITDY='7',
                   ARMCD='ARMCODE',
                   ARM='Arm Description',
                   TVSTRL='7 days after 1st dose',
                   TVENRL='21 days after 1st dose'
)


# load spec functionality to be handled by metadata team?
sdtm_spec <- load_spec('inst/specs/SDTM_spec.xlsx')
adam_spec <- load_spec('inst/specs/ADaM_spec.xlsx')


test_that("ADaM: Messages are raised according to variable core category and populated values or variable presence.", {

  expect_error(
    check_core(spec. = adam_spec,
               dataset. = t_adae,
               ds_name. = 'ADAE',
               var_categ. = 'req'
    )
  )

  for (var_categ in c('perm', 'cond')){
    expect_warning(
      check_core(spec. = adam_spec,
                 dataset. = t_adae,
                 ds_name. = 'ADAE',
                 var_categ. = var_categ
      )
    )
  }
}
)


test_that("SDTM: Messages are raised according to variable core category and populated values or variable presence.", {

  expect_error(
    check_core(spec. = sdtm_spec,
               dataset. = t_ds,
               ds_name. = 'DS',
               var_categ. = 'req'
    ), "are not present"
  )

  expect_error(
    check_core(spec. = sdtm_spec,
               dataset. = t_ds,
               ds_name. = 'DS',
               var_categ. = 'exp'
    ), "are not present"
  )

  expect_warning(
    check_core(spec. = sdtm_spec,
               dataset. = t_ds,
               ds_name. = 'DS',
               var_categ. = 'perm'
    )
  )

  expect_silent(
    check_core(spec. = sdtm_spec,
               dataset. = t_tv,
               ds_name. = 'TV'
    )
  )
}
)

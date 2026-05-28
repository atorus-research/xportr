# label Test 4: xportr_label: Reports metadata variables not in dataset

    Code
      xportr_label(adsl, metadata, domain = "adsl", verbose = "warn")
    Message
      
      -- Variables in metadata not found in dataset. --
      
      v 2 metadata variables skipped
    Condition
      Warning:
      Variable(s) present in `metadata` but don't exist in dataframe.
      x Problem with `BRTHDT` and `TRT01A`
    Output
        USUBJID
      1    1001
      2    1002
      3    1003

# label Test 5: xportr_label: gives proper verbose output for long label

    Code
      xportr_label(adsl, metadata, domain = "adsl", verbose = "warn")
    Condition
      Warning:
      Length of variable label must be 40 characters or less.
      x Problem with `USUBJID`.
    Output
        USUBJID SITEID AGE SEX
      1    1001      1  63   M
      2    1002      2  35   F
      3    1003      3  27   M

---

    Code
      xportr_label(adsl, metadata, domain = "adsl", verbose = "message")
    Message
      Length of variable label must be 40 characters or less.
      x Problem with `USUBJID`.
    Output
        USUBJID SITEID AGE SEX
      1    1001      1  63   M
      2    1002      2  35   F
      3    1003      3  27   M

---

    Code
      xportr_label(adsl, metadata, domain = "adsl", verbose = "stop")
    Condition
      Error in `xportr_logger()`:
      ! Length of variable label must be 40 characters or less.
      x Problem with `USUBJID`.


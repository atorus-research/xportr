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


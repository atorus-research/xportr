# order Test 12: Reports variables in metadata but missing from dataset

    Code
      xportr_order(adsl, metadata, domain = "adsl", verbose = "warn")
    Message
      -- All variables in dataset are found in `metadata` --
      
      -- All variables in dataset are ordered --
      
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


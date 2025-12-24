# length Test 12: Throws message when metadata variables not present in dataset

    Code
      xportr_length(adsl, metadata, domain = "adsl", verbose = "message")
    Message
      -- Variable lengths missing from metadata. --
      
      v 1 lengths resolved `BRTHDT`
      
      -- Variables in metadata not found in dataset. --
      
      v 2 metadata variables skipped
      Variable(s) present in `metadata` but don't exist in dataframe.
      x Problem with `BRTHDT` and `TRT01A`
    Output
        USUBJID
      1    1001
      2    1002
      3    1003


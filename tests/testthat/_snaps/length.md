# length Test 12: Throws message when metadata variables not present in dataset

    Code
      xportr_length(adsl, metadata, domain = "adsl", verbose = "message")
    Message
      -- Variables in metadata not found in dataset. --
      
      v 1 metadata variables skipped
      Variable(s) present in `metadata` but don't exist in dataframe.
      x Problem with `BRTHDT`
    Output
        USUBJID
      1    1001
      2    1002
      3    1003


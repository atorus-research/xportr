# xportr_write Test 15: Large file sizes are reported and warned

    Code
      xportr_write(large_df, path = tmp)
    Condition
      Warning:
      i xpt file size is: 8.01GB.
      x XPT file sizes should not exceed 5GB. It is recommended you call `xportr_write` with `max_size_gb` set to 5 or less to split the file into smaller files.


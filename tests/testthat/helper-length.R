# Custom expect function to test result of xportr_length
expect_attr_width <- function(result, metadata_length) {
  test_widths <- map(
    colnames(result), ~attributes(result[[.x]]) %>% pluck("width")
  ) %>%
    unlist() == metadata_length

  test_widths %>% all() %>% expect_true()
  invisible(result)
}

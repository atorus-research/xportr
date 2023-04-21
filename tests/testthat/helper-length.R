# Custom expect function to test result of xportr_length
expect_attr_width <- function(result, metadata_length) {
  widths <- map(colnames(result), ~attributes(result[[.x]]) %>% pluck("width"))
  (widths %>%
    unlist() == metadata_length) %>% all() %>%
    expect_true()
  invisible(result)
}

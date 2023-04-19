# Re-usable data for sample ADSL table
minimal_adsl <- data.frame(
  USUBJID = c(1001, 1002, 1003),
  BRTHDT = c(1, 1, 2)
)

# Re-usable data from sample metadata for ADSL
minimal_length_metadata <- data.frame(
  dataset = c("adsl", "adsl"),
  variable = c("USUBJID", "BRTHDT"),
  length = c(20, 8)
)

# Custom expect function to test result of xportr_length
expect_attr_width <- function(result, metadata_length) {
  (purrr::map(result, attributes) %>%
     purrr::pluck("width") %>% unlist() == metadata_length) %>% all() %>%
    expect_true()
  invisible(result)
}

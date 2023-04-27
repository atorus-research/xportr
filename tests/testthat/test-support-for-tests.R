test_that("minimal_table: builds minimal data frame with data", {
  minimal_table(31) %>%
    NROW() %>%
    expect_equal(31)

  (colnames(minimal_table(31)) %in% c("x", "y")) %>%
    all() %>%
    expect_true()
})

test_that("minimal_metadata: builds minimal metadata data frame", {
  sample_metadata <- minimal_metadata(
    dataset = TRUE,
    length = TRUE,
    label = TRUE,
    type = TRUE,
    format = TRUE,
    order = TRUE
  )

  (sample_metadata$variable %in% c("x", "y", "z", "a", "b", "c", "d")) %>%
    all() %>%
    expect_true()
})

test_that("minimal_metadata: columns in minimal_table are all in metadata", {
  sample_data <- minimal_table(31, cols = c("x", "y", "z", "a", "b", "c", "d"))
  sample_metadata <- minimal_metadata(dataset = TRUE)
  (colnames(sample_data) %in% sample_metadata$variable) %>%
    all() %>%
    expect_true()
})

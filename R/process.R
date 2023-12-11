xportr_process <- function(.df,
                           metadata = NULL,
                           domain = NULL,
                           verbose = getOption("xportr.type_verbose", "none"),
                           path,
                           strict_checks = FALSE
                           ) {
  .df %>%
    xportr_metadata(metadata, domain) %>%
    xportr_type() %>%
    xportr_length() %>%
    xportr_label() %>%
    xportr_order() %>%
    xportr_format() %>%
    xportr_df_label(dataset_spec) %>%
    xportr_write("adsl.xpt")
}

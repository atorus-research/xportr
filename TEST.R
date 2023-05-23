devtools::load_all()

df <- data.frame(x = 1, y = 2)
df_meta <- data.frame(
    dataset = "df",
    variable = c("x", "y"),
    format = c("date9.", "datetime20.")
)

# formatted_df <- xportr_format(df, metacore = df_meta)
formatted_df <- xportr_format(df, metacore = df_meta)

# Newer versions of haven attach a `haven_meta` attribute (including the table name
# and timestamps) to data read from xpt files; drop it so comparisons are stable
read_xpt_no_meta <- function(path) {
  df <- haven::read_xpt(path)
  attr(df, "haven_meta") <- NULL
  df[] <- lapply(df, function(x) {
    attr(x, "haven_meta") <- NULL
    x
  })
  df
}

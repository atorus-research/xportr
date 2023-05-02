set_metadata <- function(.df, metacore) {
  stopifnot(
    "`.df` must be a dataframe" = inherits(.df, "data.frame"),
    "`metacore` must be a dataframe or metacore object" =
      inherits(.df, "data.frame") || inherits(.df, "metacore")
  )

  structure(.df, metadata = metacore)
}

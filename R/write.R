#' Write xpt v5 transport file
#'
#' Writes a local data frame into SAS transport file of version 5. The SAS
#' transport format is an open format, as is required for submission of the data
#' to the FDA.
#'
#' @param .df A data frame to write.
#' @param path Path where transport file will be written. File name sans will be
#'   used as `xpt` name.
#' @param max_size_gb Maximum size in GB of the exported file(s). If size of xpt file exceeds the specified maximum,
#' it will split the data frame into multiple exported chunk(s).
#' @param label `r lifecycle::badge("deprecated")` Previously used to to set the Dataset label.
#' Use the `metadata` argument to set the dataset label.
#' @param strict_checks If TRUE, xpt validation will report errors and not write
#'   out the dataset. If FALSE, xpt validation will report warnings and continue
#'   with writing out the dataset. Defaults to FALSE
#' @inheritParams xportr_df_label
#' @inheritSection xportr_df_label Metadata
#'
#' @details
#'   * Variable and dataset labels are stored in the "label" attribute.
#'
#'   * SAS format are stored in the "SASformat" attribute.
#'
#'   * SAS type are based on the `metadata` attribute.
#'
#' @return A data frame. `xportr_write()` returns the input data invisibly.
#' @export
#'
#' @examples
#' adsl <- data.frame(
#'   SUBL = as.character(123, 456, 789),
#'   DIFF = c("a", "b", "c"),
#'   VAL = c("1", "2", "3"),
#'   PARAM = c("param1", "param2", "param3")
#' )
#'
#' var_spec <- data.frame(
#'   dataset = "adsl",
#'   label = "Subject-Level Analysis Dataset",
#'   data_label = "ADSL"
#' )
#' xportr_write(adsl,
#'   path = paste0(tempdir(), "/adsl.xpt"),
#'   domain = "adsl",
#'   metadata = var_spec,
#'   strict_checks = FALSE
#' )
#'
xportr_write <- function(.df,
                         path,
                         max_size_gb = NULL,
                         metadata = NULL,
                         domain = NULL,
                         strict_checks = FALSE,
                         label = deprecated()) {
  if (!missing(label)) {
    lifecycle::deprecate_warn(
      when = "0.3.2",
      what = "xportr_write(label = )",
      with = "xportr_write(metadata = )"
    )
    assert_string(label, null.ok = TRUE, max.chars = 40)
    metadata <- data.frame(dataset = domain, label = label)
  }

  ## Common section to detect default arguments

  domain <- domain %||% attr(.df, "_xportr.df_arg_")
  if (!is.null(domain)) attr(.df, "_xportr.df_arg_") <- domain

  # metadata should not be inferred from the data frame if it is not provided
  # by the user.

  ## End of common section

  assert_data_frame(.df)
  assert_string(path)
  assert_numeric(max_size_gb, null.ok = TRUE)
  assert_metadata(metadata, null.ok = TRUE)
  assert_logical(strict_checks)

  path <- normalizePath(path, mustWork = FALSE)

  name <- tools::file_path_sans_ext(basename(path))

  if (!is.null(metadata)) {
    .df <- xportr_df_label(.df, metadata = metadata, domain = domain)
  }

  if (nchar(name) > 8) {
    assert(".df file name must be 8 characters or less.", .var.name = "path")
  }

  if (is.numeric(max_size_gb) && max_size_gb <= 0) {
    assert("max_size_gb must be NULL or a numeric value greater than 0.", .var.name = "max_size_gb")
  }

  checks <- xpt_validate(.df)

  if (stringr::str_detect(name, "[^a-zA-Z0-9]")) {
    checks <- c(checks, "`.df` cannot contain any non-ASCII, symbol or underscore characters.")
  }

  if (length(checks) > 0) {
    if (!strict_checks) {
      warn(c("The following validation checks failed:", checks))
    } else {
      abort(c("The following validation checks failed:", checks))
    }
  }

  data <- as.data.frame(.df)

  tryCatch(
    {
      # If data is not split, data is just written out
      if (!is.null(max_size_gb)) {
        export_to_xpt(data, path = path, max_size_gb = max_size_gb, file_prefix = name)
      } else if (!is.null(attr(data, "_xportr.split_by_"))) {
        # If data is split, perform the split and get an index for the for loop
        split_data <- split(data, data[[attr(data, "_xportr.split_by_")]])
        split_index <- unique(data[[attr(data, "_xportr.split_by_")]])
        paths <- get_split_path(path, seq_along(split_index))
        # Iterate on the unique values of the split
        for (i in seq_along(split_index)) {
          # Write out the split data, `get_split_path` will determine file name
          write_xpt(
            split_data[[i]],
            path = paths[i], version = 5, name = name
          )
          check_xpt_size(paths[i])
        }
      } else {
        write_xpt(data, path = path, version = 5, name = name)
        check_xpt_size(path)
      }
    },
    error = function(err) {
      abort(
        paste0(
          "Error reported by haven::write_xpt, error was: \n",
          err
        )
      )
    }
  )

  invisible(data)
}

#' Figure out path for split data.
#'
#' Will append a number before the file extension to indicate the split.
#'
#' i.e. `adsl.xpt` will become `adsl1.xpt` and `adsl2.xpt`
#'
#' @param path Path variable specified by user
#' @param ind Index of split variable
#'
#' @noRd
get_split_path <- function(path, ind) {
  paste0(
    tools::file_path_sans_ext(path),
    ind,
    ".",
    tools::file_ext(path)
  )
}

#' Function to export data frame to xpt files ensuring each file does not exceed the maximum specified size in GB
#'
#' @param df A data frame to write.
#' @param max_size_gb Maximum size in GB of the exported files.
#' @param file_prefix Name of each exported file.
#'
#' @noRd

export_to_xpt <- function(.df, path, max_size_gb, file_prefix) {
  # Convert GB to bytes
  max_size_bytes <- max_size_gb * 1000^3

  temp_file <- tempfile()
  write_xpt(.df, temp_file)

  file_size <- file.info(temp_file)$size
  unlink(temp_file) # Clean up the temporary file

  dir_path <- dirname(path)

  if (file_size <= max_size_bytes) {
    chunk_counter <- 2
    file_name <- sprintf("%s.xpt", file_prefix)
    path <- file.path(dir_path, file_name)
    write_xpt(.df, path = path, version = 5, name = file_prefix)
  } else {
    total_rows <- nrow(.df)
    row_start <- 1
    chunk_counter <- 1

    while (row_start <= total_rows) {
      # Binary search to find the maximum number of rows that fit within the size limit
      low <- row_start
      high <- total_rows
      best_fit <- row_start

      while (low <= high) {
        mid <- floor((low + high) / 2)
        write_xpt(.df[row_start:mid, ], temp_file)
        file_size <- file.info(temp_file)$size

        if (file_size <= max_size_bytes) {
          best_fit <- mid
          low <- mid + 1
        } else {
          high <- mid - 1
        }

        unlink(temp_file) # Clean up the temporary file
      }

      # Write the best fitting chunk to the final file
      chunk <- .df[row_start:best_fit, ]

      file_name <- sprintf("%s%d.xpt", file_prefix, chunk_counter)
      path <- file.path(dir_path, file_name)

      write_xpt(chunk, path = path, version = 5, name = file_prefix)

      # Update counters
      row_start <- best_fit + 1
      chunk_counter <- chunk_counter + 1
    }
  }

  message("Data frame exported to ", chunk_counter - 1, " xpt files.")
}

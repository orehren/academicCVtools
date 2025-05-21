# R/format_typst_section_helpers.R

#' Combine specified columns in a data frame into a single string column
#'
#' Handles NA values according to specified prefix and separator.
#' Intended as an internal helper for format_typst_section.
#'
#' @param data_proc The input data frame/tibble.
#' @param combine_col_names Character vector of column names to combine.
#' @param combine_as Name of the new combined column (string).
#' @param combine_sep Separator string.
#' @param combine_prefix Prefix string for each non-NA item.
#'
#' @return The data frame with the combined column added and original
#'         columns removed. Returns the original data frame if
#'         `combine_col_names` is empty.
#'
#' @importFrom dplyr select all_of %>%
#' @importFrom purrr pmap_chr discard
#' @importFrom stringr str_c
#' @importFrom cli cli_warn
#'
#' @noRd
none <- NULL

.combine_columns_data <- function(data_proc, combine_col_names, combine_as, combine_sep, combine_prefix) {
  # Guard clause: If no columns to combine, return data as is
  if (length(combine_col_names) == 0) {
    return(data_proc)
  }

  # Check if combine_as name already exists and is not part of combine_cols
  if (combine_as %in% names(data_proc) && !(combine_as %in% combine_col_names)) {
    cli::cli_warn("Column specified in {.arg combine_as} ({.val {combine_as}}) already exists and is not part of {.arg combine_cols}. It will be overwritten.")
  }

  # Select only the columns to be combined to pass to pmap
  # Ensure selected columns exist (should be guaranteed by evaluate_tidyselect_safely)
  data_to_combine <- tryCatch(
    {
      data_proc %>% dplyr::select(dplyr::all_of(combine_col_names))
    },
    error = function(e) {
      # This should ideally not happen if evaluate_tidyselect_safely worked
      cli::cli_abort("Internal error: Columns specified in combine_cols not found in data.", parent = e, call. = FALSE)
      # Return original data to allow flow to continue? Or abort? Abort seems safer.
    }
  )

  # Use pmap_chr to process row-wise
  combined_strings <- purrr::pmap_chr(
    data_to_combine,
    function(...) {
      vals <- list(...)
      vals <- purrr::discard(vals, is.na)
      if (length(vals) == 0) {
        return(NA_character_)
      }
      stringr::str_c(combine_prefix, vals, collapse = combine_sep)
    }
  )

  # Add the new combined column to the original data frame
  data_proc[[combine_as]] <- combined_strings

  # Remove original combined columns
  # Ensure columns actually exist before trying to remove them
  cols_to_remove <- intersect(combine_col_names, names(data_proc))
  if (length(cols_to_remove) > 0) {
    data_proc <- data_proc %>% dplyr::select(-dplyr::all_of(cols_to_remove))
  }

  return(data_proc)
}


#' Exclude specified columns from a data frame
#'
#' Intended as an internal helper for format_typst_section.
#'
#' @param data_proc The input data frame/tibble.
#' @param exclude_col_names Character vector of column names to exclude.
#'
#' @return The data frame without the specified columns. Returns the
#'         original data frame if `exclude_col_names` is empty or NULL.
#'
#' @importFrom dplyr select all_of
#' @importFrom cli cli_inform
#'
#' @noRd
.exclude_columns_data <- function(data_proc, exclude_col_names) {
  # Guard clause: If no columns to exclude, return data as is
  if (is.null(exclude_col_names) || length(exclude_col_names) == 0) {
    return(data_proc)
  }

  # Identify which of the columns to exclude actually exist in the data
  # (Some might have been removed by combine_cols already)
  exclude_col_names_exist <- intersect(exclude_col_names, names(data_proc))

  # Exclude existing columns if any were found
  if (length(exclude_col_names_exist) > 0) {
    data_proc <- data_proc %>% dplyr::select(-dplyr::all_of(exclude_col_names_exist))
  }

  # Inform user if some requested columns were not found (optional)
  if (length(exclude_col_names) > length(exclude_col_names_exist)) {
    cols_not_found <- setdiff(exclude_col_names, exclude_col_names_exist)
    cli::cli_inform("Note: Column(s) requested in {.arg exclude_cols} were not found or already removed: {.val {cols_not_found}}")
  }

  return(data_proc)
}


#' Handle NA values in a single row based on na_action
#' @param row_data A named list representing a row.
#' @param na_action String: "omit", "keep", or "string".
#' @return A named list with NAs processed.
#' @noRd
#' @importFrom purrr discard map
#' @importFrom rlang expr
.handle_na_in_row <- function(row_data, na_action) {
  if (na_action == "omit") {
    return(
      purrr::discard(
        row_data, is.na
      )
    )
  }

  if (na_action == "keep") {
    return(
      purrr::map(
        row_data, ~ if (is.na(.x)) rlang::expr(none) else .x
      )
    )
  }

  # Assume na_action == "string" (match.arg happened earlier)
  return(
    purrr::map(
      row_data, ~ if (is.na(.x)) "NA" else as.character(.x)
    )
  )
}


#' Format a single key-value pair for Typst dictionary
#' @param key The name of the element (string).
#' @param value The value of the element (can be rlang::expr(none)).
#' @return A formatted string like '"key": "value"' or '"key": none'.
#' @noRd
#' @importFrom stringr str_glue
#' @importFrom rlang expr
.format_single_kv_pair <- function(key, value) {
  key_fmt <- stringr::str_glue("{key}")
  value_fmt <- ""

  # This if/else is now the only nesting here
  if (identical(value, rlang::expr(none))) {
    value_fmt <- "none"
  } else {
    value_str <- as.character(value)
    value_esc <- .escape_typst_string(value_str) # Assumes .escape_typst_string exists
    value_fmt <- stringr::str_glue("\"{value_esc}\"")
  }
  return(stringr::str_glue("{key_fmt}: {value_fmt}"))
}


#' Process a single row for Typst output (flatter version)
#' @param ... Arguments representing the row data.
#' @param .na_action The NA action (passed via pmap).
#' @return A single string for the Typst function call or "".
#' @noRd
#' @importFrom purrr map_chr discard
#' @importFrom stringr str_c
.process_single_row_for_typst <- function(..., .na_action) {
  row_data <- list(...)
  processed_row_data <- .handle_na_in_row(row_data, .na_action)
  if (length(processed_row_data) == 0) return("")

  kv_pairs <- purrr::map_chr(
    seq_along(processed_row_data),
    ~ .format_single_kv_pair(
      key = names(processed_row_data)[.x],
      value = processed_row_data[[.x]]
    )
  )
  return(paste0("(", stringr::str_c(kv_pairs, collapse = ", "), ")"))

}


#' Generate individual Typst function call strings for each row of data
#' Delegates row processing to helpers for flatter structure.
#' @param data_proc The final processed data frame/tibble.
#' @param typst_func The Typst function name (string starting with #).
#' @param na_action How to handle NA values ("omit", "keep", "string").
#' @return A character vector of Typst function calls, filtered for empty strings.
#' @noRd
#' @importFrom purrr pmap_chr
.generate_typst_rows <- function(data_proc, typst_func, na_action, output_mode) {
  if (nrow(data_proc) == 0) return(character(0))

  dictionary_strings <- purrr::pmap_chr(
    data_proc,
    .f = .process_single_row_for_typst,
    .na_action = na_action
  )

  dictionary_strings <- dictionary_strings[dictionary_strings != ""]
  if (length(dictionary_strings) == 0) return(character(0))

  if (output_mode == "rowwise") {
    return(paste0(typst_func, dictionary_strings))
  } else {
    return(dictionary_strings)
  }
}

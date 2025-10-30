# R/validation_helpers.R

#' Helper function to check for Pandoc availability
#' @return `TRUE` if Pandoc is found, otherwise `FALSE`.
#' @noRd
#' @export
.is_pandoc_available <- function() {
  Sys.which("pandoc") != ""
}

#' Validate arguments for create_publication_list
#'
#' @param bib_file Path to the .bib file.
#' @param author_name Author's full name.
#' @param csl_file Path to the .csl file.
#' @param group_labels Named character vector for group labels.
#' @param default_label Default label string.
#' @param group_order Optional character vector for sort order.
#' @param pandoc_path Optional path to Pandoc executable.
#' @param author_highlight_markup Typst markup string.
#' @param typst_func_name Typst function name.
#'
#' @return `TRUE` (invisibly) if all checks pass.
#' @importFrom checkmate assert_file_exists assert_string assert_character assert_named
#' @noRd
#' @export
.validate_create_publication_list_args <- function(
    bib_file, author_name, csl_file, group_labels, default_label,
    group_order, pandoc_path, author_highlight_markup, typst_func_name) {
  checkmate::assert_file_exists(bib_file, extension = "bib")
  checkmate::assert_string(author_name, min.chars = 1)
  checkmate::assert_file_exists(csl_file, extension = "csl")
  checkmate::assert_character(group_labels, min.len = 1, names = "unique")
  checkmate::assert_string(default_label, min.chars = 1)
  if (!is.null(group_order)) {
    checkmate::assert_character(group_order, unique = TRUE)
  }
  if (!is.null(pandoc_path)) {
    checkmate::assert_string(pandoc_path, min.chars = 1)
  }
  checkmate::assert_string(
    author_highlight_markup,
    pattern = "%s"
  )
  checkmate::assert_string(
    typst_func_name,
    pattern = "^[a-zA-Z0-9_-]+$"
  )
  invisible(TRUE)
}

#' Validate arguments for format_typst_section
#'
#' @param data A data frame or tibble.
#' @param typst_func The name of the Typst function.
#' @param combine_cols Tidyselect expression.
#' @param combine_as Name of the new combined column.
#' @param combine_sep Separator for combined columns.
#' @param combine_prefix Prefix for combined column values.
#' @param exclude_cols Tidyselect expression.
#' @param na_action How to handle `NA` values.
#' @param output_mode Output structure.
#'
#' @return `TRUE` (invisibly) if all checks pass.
#' @importFrom checkmate assert_data_frame assert_string assert_character assert_choice
#' @importFrom rlang quo_is_null
#' @noRd
#' @export
.validate_format_typst_section_args <- function(
    data, typst_func, combine_cols, combine_as, combine_sep,
    combine_prefix, exclude_cols, na_action, output_mode) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(typst_func, pattern = "^#")
  checkmate::assert_string(combine_as)
  checkmate::assert_string(combine_sep)
  checkmate::assert_string(combine_prefix)
  checkmate::assert_choice(na_action, c("omit", "keep", "string"))
  checkmate::assert_choice(output_mode, c("rowwise", "array"))
  invisible(TRUE)
}

#' Validate arguments for load_cv_sheets
#' @noRd
#' @export
.validate_load_cv_sheets_args <- function(doc_identifier, sheets_to_load, ...) {
  # Validate doc_identifier
  checkmate::assert_string(doc_identifier, min.chars = 1)

  # Validate sheets_to_load
  if (is.character(sheets_to_load)) {
    checkmate::assert_character(sheets_to_load, min.chars = 1, min.len = 1)
  } else {
    checkmate::assert_list(sheets_to_load, types = "character", names = "unique")
  }
}

#' Validate arguments for read_cv_sheet
#' @noRd
#' @export
.validate_read_cv_sheet_args <- function(
    doc_identifier, sheet_name, na_strings, col_types, trim_ws) {
  checkmate::assert_string(doc_identifier, min.chars = 1)
  checkmate::assert_string(sheet_name, min.chars = 1)
  checkmate::assert_character(na_strings, null.ok = FALSE)
  if (!is.null(col_types)) {
    checkmate::assert_atomic(col_types)
  }
  checkmate::assert_logical(trim_ws, len = 1, any.missing = FALSE)
}

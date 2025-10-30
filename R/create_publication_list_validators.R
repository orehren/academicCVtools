# R/create_publication_list_validators.R

#' Validate arguments for create_publication_list
#'
#' Uses assertions from the `checkmate` package to validate all arguments
#' for the `create_publication_list` function. Aborts with an informative
#' error message if any validation fails.
#'
#' @param bib_file Path to the .bib file. Must be a single, non-empty string
#'   and the file must exist.
#' @param author_name Author's full name. Must be a single, non-empty string.
#' @param csl_file Path to the .csl file. Must be a single, non-empty string
#'   and the file must exist.
#' @param group_labels Named character vector for group labels. Must be a
#'   named character vector, names must be unique.
#' @param default_label Default label string. Must be a single, non-empty string.
#' @param group_order Optional character vector for sort order. Must be `NULL`
#'   or a character vector with unique items.
#' @param pandoc_path Optional path to Pandoc executable. Must be `NULL` or a
#'   single, non-empty string.
#' @param author_highlight_markup Typst markup string. Must be a single,
#'   non-empty string containing the `%s` placeholder.
#' @param typst_func_name Typst function name. Must be a single, non-empty
#'   string matching a specific pattern.
#'
#' @return `TRUE` (invisibly) if all checks pass.
#' @importFrom checkmate assert_string assert_file_exists assert_character
#'   assert_named assert_null
#' @noRd
.validate_create_publication_list_args <- function(
    bib_file, author_name, csl_file, group_labels, default_label,
    group_order, pandoc_path, author_highlight_markup, typst_func_name) {
  # --- Input Validation ---
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
    pattern = "%s",
    fixed = TRUE
  )
  checkmate::assert_string(
    typst_func_name,
    pattern = "^[a-zA-Z0-9_-]+$"
  )
  invisible(TRUE)
}

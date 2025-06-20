# In R/validation_helpers.R

#' Validate arguments for read_cv_sheet function
#' Uses checkmate for assertions. Aborts on failure.
#' @param doc_identifier See read_cv_sheet documentation.
#' @param sheet_name See read_cv_sheet documentation.
#' @param na_strings See read_cv_sheet documentation.
#' @param col_types See read_cv_sheet documentation.
#' @param trim_ws See read_cv_sheet documentation.
#' @return Invisibly returns TRUE if all checks pass.
#' @noRd
#' @importFrom checkmate assert_string assert_character assert_logical assert_atomic test_null
.validate_read_cv_sheet_args <- function(doc_identifier, sheet_name, na_strings, col_types, trim_ws) {
  checkmate::assert_string(doc_identifier, min.chars = 1, .var.name = "doc_identifier")
  checkmate::assert_string(sheet_name, min.chars = 1, .var.name = "sheet_name")
  checkmate::assert_character(na_strings, any.missing = FALSE, null.ok = FALSE, min.len = 0, .var.name = "na_strings")

  # --- Korrigierte Prüfung für col_types ---
  # Check if it's NULL OR an atomic vector
  is_null <- checkmate::test_null(col_types)
  is_atomic <- checkmate::test_atomic(col_types) # test_atomic allows vectors

  if (!is_null && !is_atomic) {
    # Use assert(..., .var.name) inside cli_abort for consistent naming
    checkmate::assert(
      is_null || is_atomic,
      "Argument {.arg col_types} must be NULL or an atomic vector (like a string).",
      .var.name = "col_types"
    )
  }
  # --- Ende der Korrektur ---

  checkmate::assert_logical(trim_ws, len = 1, any.missing = FALSE, null.ok = FALSE, .var.name = "trim_ws")

  invisible(TRUE)
}



#' Validate arguments for load_cv_sheets function
#' Uses checkmate for assertions. Aborts on failure.
#' @param doc_identifier See load_cv_sheets documentation.
#' @param sheets_to_load See load_cv_sheets documentation.
#' @param ... Dots argument (not validated here).
#' @return Invisibly returns TRUE if all checks pass.
#' @noRd
#' @importFrom checkmate assert_string assert check_list check_character check_named test_list test_character test_atomic assert_character assert_list assert_vector assert_names test_names
#' @importFrom rlang is_named is_list
.validate_load_cv_sheets_args <- function(doc_identifier, sheets_to_load, ...) {
  checkmate::assert_string(doc_identifier, min.chars = 1, .var.name = "doc_identifier")

  is_special_wildcard <- checkmate::test_string(sheets_to_load, fixed = "*")

  if (!is_special_wildcard) {
    is_list_or_atomic <- rlang::is_list(sheets_to_load) || is.atomic(sheets_to_load)
    checkmate::assert(is_list_or_atomic,
                      "Argument {.arg sheets_to_load} must be the string \"*\", a list, or an atomic vector.",
                      .var.name = "sheets_to_load")

    is_named_arg <- rlang::is_named(sheets_to_load)

    if (is_named_arg) {
      checkmate::assert_character(names(sheets_to_load), min.chars = 1, any.missing = FALSE, unique = TRUE, null.ok = FALSE, .var.name = "names(sheets_to_load) (Sheet Names)")
      target_names <- unlist(sheets_to_load)
      checkmate::assert_character(target_names, min.chars = 1, any.missing = FALSE, null.ok = FALSE, .var.name = "values in sheets_to_load (Target List Names)")
      are_valid_r_names <- checkmate::test_names(target_names, type = "strict")
      checkmate::assert(are_valid_r_names, "Values in named {.arg sheets_to_load} (Target List Names) must be syntactically valid R names.", .var.name = "values in sheets_to_load")
      if (rlang::is_list(sheets_to_load)) {
        checkmate::assert_list(sheets_to_load, types = "character", min.len = 0, .var.name = "sheets_to_load (list elements type)")
        all_elements_scalar <- all(sapply(sheets_to_load, checkmate::test_string))
        checkmate::assert(all_elements_scalar, "All elements in named list {.arg sheets_to_load} must be single strings.", .var.name = "sheets_to_load elements scalar")
      } else {
        checkmate::assert_character(sheets_to_load, min.len = 0, any.missing = FALSE, .var.name = "sheets_to_load (vector elements)")
      }
    } else {
      is_unnamed_list_of_chars <- checkmate::test_list(sheets_to_load, types = "character", names = "unnamed", min.len = 0)
      is_unnamed_char_vector <- checkmate::test_character(sheets_to_load, names = "unnamed", min.len = 0)

      checkmate::assert(is_unnamed_list_of_chars || is_unnamed_char_vector,
                        "If {.arg sheets_to_load} is unnamed, it must be a list of character strings or a character vector.",
                        .var.name = "sheets_to_load (unnamed type)")

      # Elements (sheet names) must not be empty strings
      # unlist works for both lists of characters and character vectors
      sheet_name_values <- unlist(sheets_to_load)
      checkmate::assert_character(sheet_name_values, min.chars = 1, any.missing = FALSE,
                                  .var.name = "sheet names in sheets_to_load (unnamed)")
    }
  }
  invisible(TRUE)
}


#' Validate arguments for create_publication_list function
#' Uses checkmate for assertions. Aborts on failure.
#' @param bib_file Path to the .bib file.
#' @param author_name Name of the author to highlight.
#' @param csl_file Path to the .csl file.
#' @param group_labels Named list/vector mapping BibTeX types to display labels.
#' @param default_label Label for unmapped BibTeX types.
#' @param group_order Optional vector for sorting group labels.
#' @param pandoc_path Optional path to Pandoc executable.
#' @param author_highlight_markup Typst markup for highlighting.
#' @param typst_func_name Name of the Typst function to call.
#' @return Invisibly returns TRUE if all checks pass.
#' @noRd
#' @importFrom checkmate assert_file_exists assert_string assert_list assert_character assert_true test_list test_character
#' @importFrom rlang check_installed is_named
.validate_create_publication_list_args <- function(
    bib_file, author_name, csl_file, group_labels, default_label,
    group_order, pandoc_path, author_highlight_markup, typst_func_name
  ) {
  # Check dependencies first
  rlang::check_installed("jsonlite", reason = "to parse Pandoc's JSON output.")

  # Check file existence and readability
  checkmate::assert_file_exists(bib_file, access = "r", extension = "bib", .var.name = "bib_file")
  checkmate::assert_file_exists(csl_file, access = "r", extension = "csl", .var.name = "csl_file")

  # Check required strings
  checkmate::assert_string(author_name, min.chars = 1, .var.name = "author_name")
  checkmate::assert_string(author_highlight_markup, pattern = "%s", .var.name = "author_highlight_markup")
  checkmate::assert_string(default_label, min.chars = 1, .var.name = "default_label")
  checkmate::assert_string(typst_func_name, min.chars = 1, .var.name = "typst_func_name")

  # Check group_labels (must be named list or named character vector of strings)
  is_list_check <- checkmate::test_list(group_labels, types = "character", names = "strict")
  is_char_vec_check <- checkmate::test_character(group_labels, names = "strict")
  checkmate::assert(is_list_check || is_char_vec_check,
    "Argument {.arg group_labels} must be a strictly named list or character vector containing only character strings.",
    .var.name = "group_labels"
  )
  # Additionally check names and elements are non-empty
  checkmate::assert_character(names(group_labels), min.chars = 1, any.missing = FALSE, unique = TRUE, .var.name = "names(group_labels)")
  checkmate::assert_character(unlist(group_labels), min.chars = 1, any.missing = FALSE, .var.name = "values in group_labels")

  # Check group_order (optional character vector, must be unique if provided)
  checkmate::assert_character(group_order, null.ok = TRUE, unique = TRUE, any.missing = FALSE, .var.name = "group_order")

  # Check pandoc_path (optional string, must exist if provided)
  checkmate::assert_string(pandoc_path, null.ok = TRUE, .var.name = "pandoc_path")
  if (!is.null(pandoc_path)) {
    # If path is given, check if it's an existing file (or directory?)
    # assert_access checks for executable, might be too strict if it's just a path variable
    # Let's use assert_path_for_output which checks if parent dir exists, or assert_directory / assert_file
    # Simpler: rely on .validate_executable_found called later for actual check.
    # Just check if it's a non-empty string if provided.
    checkmate::assert_string(pandoc_path, min.chars = 1, .var.name = "pandoc_path (if provided)")
  }

  invisible(TRUE)
}


#' Check Pandoc JSON structure
#' @param data Parsed JSON data (list)
#' @return TRUE if structure is valid, otherwise an error message string.
#' @noRd
.check_pandoc_json_structure <- function(data) {
  if (!checkmate::test_list(data, names = "named") || !checkmate::test_subset(c("meta", "blocks"), names(data))) {
    return("JSON missing top-level 'meta' or 'blocks', or is not a named list.")
  }
  if (!checkmate::test_list(data$meta, names = "named") || !checkmate::test_subset("references", names(data$meta))) {
    return("JSON 'meta' is not a named list or missing 'references'.")
  }
  if (!checkmate::test_list(data$meta$references, names = "named") || !checkmate::test_subset("c", names(data$meta$references))) {
    return("JSON 'meta$references' is not a named list or missing 'c'.")
  }
  if (!checkmate::test_list(data$meta$references$c)) {
    return("JSON 'meta$references$c' (the list of references) is not a list.")
  }
  if (!checkmate::test_list(data$blocks)) {
    return("JSON 'blocks' is not a list.")
  }
  return(TRUE)
}


#' Validate arguments for format_typst_section function
#' Uses checkmate for assertions. Aborts on failure.
#' @param data The input data frame/tibble.
#' @param typst_func The name of the Typst function.
#' @param combine_cols Tidyselect expression for columns to combine.
#' @param combine_as Name for the combined column.
#' @param combine_sep Separator for combining.
#' @param combine_prefix Prefix for combined items.
#' @param exclude_cols Tidyselect expression for columns to exclude.
#' @param na_action How to handle NA values.
#' @param output_mode Mode of output: "rowwise" or "array".
#' @return Invisibly returns TRUE if all checks pass.
#' @noRd
#' @importFrom checkmate assert_data_frame assert_string assert_character assert_choice assert_multi_class assert test_list
#' @importFrom rlang enquo quo_is_null is_quosure
.validate_format_typst_section_args <- function(
    data, typst_func, combine_cols, combine_as, combine_sep,
    combine_prefix, exclude_cols, na_action, output_mode
  ) {
  # Check data (must be data frame, allow 0 rows but not NULL)
  checkmate::assert_data_frame(data, null.ok = FALSE, .var.name = "data")

  # Check typst_func (string starting with #)
  checkmate::assert_string(typst_func, pattern = "^#", .var.name = "typst_func")

  # Check combine arguments (single strings)
  checkmate::assert_string(combine_as, min.chars = 1, .var.name = "combine_as")
  checkmate::assert_string(combine_sep, .var.name = "combine_sep") # Allow empty string
  checkmate::assert_string(combine_prefix, .var.name = "combine_prefix") # Allow empty string
  checkmate::assert_choice(output_mode, choices = c("rowwise", "array"), .var.name = "output_mode")

  # Check na_action (must be one of the allowed choices)
  # Note: match.arg happens in the main function, assert_choice checks here
  # checkmate::assert_choice(na_action, choices = c("omit", "keep", "string"), .var.name = "na_action")

  # Check quosures for combine_cols and exclude_cols
  # We capture them in the main function, so we just check their basic type here
  # A quosure is a specific type of formula or language object
  # Check if it's a quosure OR NULL (if argument was not provided/NULL)
  # Note: Direct checkmate assertion on quosures is tricky.
  # We rely on the later eval_select to catch errors in the expression itself.
  # We can check if the captured object is of a type that eval_select might handle.
  # This check is optional as eval_select provides good errors.
  # checkmate::assert(
  #     rlang::is_quosure(combine_cols) || rlang::is_null(combine_cols),
  #    "Internal check failed: combine_cols not a quosure or NULL."
  # )
  # checkmate::assert(
  #     rlang::is_quosure(exclude_cols) || rlang::is_null(exclude_cols),
  #     "Internal check failed: exclude_cols not a quosure or NULL."
  # )

  invisible(TRUE)
}


#' Validate arguments for format_date_range function
#' Uses checkmate for assertions. Aborts on failure.
#' @param data The input data frame.
#' @param start_col Name of the start date column.
#' @param end_col Name of the end date column.
#' @param output_col Name for the new combined date column.
#' @param output_format String format for the dates (e.g., "%Y", "%m/%Y").
#' @param sep Separator string between start and end dates.
#' @param ongoing_label Label for ongoing items.
#' @noRd
#' @importFrom checkmate assert_data_frame assert_string assert_subset assert_choice
.validate_format_date_range_args <- function(data, start_col, end_col, output_col, output_format, sep, ongoing_label) {

  # Check basic types
  checkmate::assert_data_frame(data, .var.name = "data")
  checkmate::assert_string(start_col, min.chars = 1, .var.name = "start_col")
  checkmate::assert_string(end_col, min.chars = 1, .var.name = "end_col")
  checkmate::assert_string(output_col, min.chars = 1, .var.name = "output_col")
  checkmate::assert_string(output_format, min.chars = 1, .var.name = "output_format")
  checkmate::assert_string(sep, .var.name = "sep") # Allow empty
  checkmate::assert_string(ongoing_label, min.chars = 1, .var.name = "ongoing_label")

  # Check if columns exist in the data frame
  checkmate::assert_subset(c(start_col, end_col), choices = names(data),
                           .var.name = "start_col/end_col")

  # A simple check for valid format specifiers
  # This is not exhaustive but catches common errors
  if (!grepl("^[%Ymd]+[/.-]*[%Ymd]*[/.-]*[%Ymd]*$", output_format)) {
    cli::cli_warn("Argument {.arg output_format} {.val {output_format}} seems unusual. Ensure it uses valid strftime specifiers like %Y, %m, %d.")
  }
}

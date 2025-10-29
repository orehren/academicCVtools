# R/create_publication_list.R

#' Create a formatted publication list for Typst from a .bib file
#'
#' This function provides a comprehensive workflow for generating a publication
#' list suitable for an academic CV in Typst. It reads a bibliography file,
#' processes it using Pandoc and a CSL stylesheet, and then groups, sorts, and
#' formats the entries into a Typst-ready code block.
#'
#' @details
#' The process follows these key steps:
#' 1.  **Argument Validation:** Ensures all inputs are correct and files exist.
#' 2.  **Pandoc Execution:** Calls Pandoc with `citeproc` to convert the `.bib`
#'     file into a structured JSON output.
#' 3.  **JSON Parsing & Validation:** Parses the JSON and validates its structure.
#' 4.  **Data Extraction:** Extracts metadata (e.g., citation key, year) and the
#'     formatted citation strings from the JSON.
#' 5.  **Processing:** Joins the metadata and formatted strings, assigns group
#'     labels (e.g., "Journal Article"), and highlights the specified author's name.
#' 6.  **Sorting:** Sorts the publications by group and then by year (descending).
#' 7.  **Typst Output:** Generates the final Typst code block, which calls a
#'     Typst function with the processed data.
#'
#' @param bib_file Path to the `.bib` bibliography file.
#' @param author_name The full name of the author to highlight in publications.
#' @param csl_file Path to the `.csl` citation style file for formatting.
#' @param group_labels A named character vector to map BibTeX entry types
#'   (e.g., `article`) to user-friendly display labels (e.g., `"Journal Article"`).
#'   Defaults to a standard set of academic publication types.
#' @param default_label The display label for any entry types not found in
#'   `group_labels`. Defaults to `"Other"`.
#' @param group_order An optional character vector specifying the desired
#'   display order of the `group_labels`. If `NULL`, groups are sorted
#'   alphabetically.
#' @param author_highlight_markup The Typst markup to apply for highlighting the
#'   author. Must contain a `%s` placeholder for the author's name.
#'   Defaults to `"#strong[%s]"`, which makes the name bold.
#' @param typst_func_name The name of the Typst function that will receive the
#'   publication data. Defaults to `"publication-list"`.
#' @param pandoc_path An optional path to the Pandoc executable. If `NULL`,
#'   Pandoc is assumed to be in the system's PATH.
#'
#' @return A single character string containing a Typst code block (` ```{=typst} ... ``` `).
#'   If no valid publication entries are found, it returns an empty Typst array block.
#'   This output is intended to be used in a Quarto document with `output: asis`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # --- Setup: Create dummy files for a reproducible example ---
#' bib_content <- c(
#'   "@article{Author2023, author={Jane Doe}, title={A great paper}, year={2023}}",
#'   "@inproceedings{Author2022, author={Jane Doe}, title={A conference talk}, year={2022}}"
#' )
#' bib_file <- tempfile(fileext = ".bib")
#' writeLines(bib_content, bib_file)
#'
#' csl_content <- '<?xml version="1.0" encoding="utf-8"?>
#' <style xmlns="http://purl.org/net/xbiblio/csl" class="in-text" version="1.0">
#'   <info><title>Simple CSL</title><id>simple-csl</id></info>
#'   <bibliography><layout><text variable="title"/></layout></bibliography>
#' </style>'
#' csl_file <- tempfile(fileext = ".csl")
#' writeLines(csl_content, csl_file)
#'
#' # --- Basic Usage ---
#' # This generates a Typst block with two publications, highlighting "Jane Doe".
#' typst_output <- create_publication_list(
#'   bib_file = bib_file,
#'   author_name = "Jane Doe",
#'   csl_file = csl_file
#' )
#' cat(typst_output)
#'
#' # --- Custom Grouping and Order ---
#' custom_labels <- c(article = "Peer-Reviewed Papers", inproceedings = "Talks")
#' custom_order <- c("Peer-Reviewed Papers", "Talks")
#'
#' typst_output_custom <- create_publication_list(
#'   bib_file = bib_file,
#'   author_name = "Jane Doe",
#'   csl_file = csl_file,
#'   group_labels = custom_labels,
#'   group_order = custom_order,
#'   author_highlight_markup = "#emph[%s]" # Italicize the author
#' )
#' cat(typst_output_custom)
#'
#' # --- Cleanup ---
#' unlink(bib_file)
#' unlink(csl_file)
#' }
create_publication_list <- function(
    bib_file,
    author_name,
    csl_file,
    group_labels = c(
      article = "Journal Article", inproceedings = "Conference Paper",
      incollection = "Book Chapter", book = "Book", phdthesis = "PhD Thesis",
      mastersthesis = "Masters Thesis", techreport = "Technical Report",
      unpublished = "In Preparation", misc = "Miscellaneous"
    ),
    default_label = "Other",
    group_order = NULL,
    pandoc_path = NULL,
    author_highlight_markup = "#strong[%s]",
    typst_func_name = "publication-list"
  ) {
  # ============================================================================
  # Phase 0: Validate Arguments
  # ============================================================================
  # Call the dedicated validation function (defined in validation_helpers.R)
  # It uses checkmate::assert_* and aborts on failure.
  .validate_create_publication_list_args(
    bib_file, author_name, csl_file, group_labels, default_label,
    group_order, pandoc_path, author_highlight_markup, typst_func_name
  )

  # ============================================================================
  # Phase 1: Input Modification & Setup
  # ============================================================================
  # Handle duplicates in group_order (safe now after validation)
  if (!is.null(group_order) && anyDuplicated(group_order)) {
    # Warning removed as assert_character(unique=TRUE) should handle this
    group_order <- unique(group_order)
  }
  # Validate/find pandoc executable (safe now after validation)
  pandoc_cmd <- .validate_executable_found("pandoc", pandoc_path, "pandoc_path")
  empty_result_string <- sprintf("```{=typst}\n#%s(())\n```", typst_func_name)

  # ============================================================================
  # Phase 2: Call Pandoc
  # ============================================================================
  cli::cli_inform("Running Pandoc to format bibliography as JSON...")
  pandoc_json_output <- .call_pandoc_json(bib_file, csl_file, pandoc_cmd)
  if (is.null(pandoc_json_output)) {
    return(empty_result_string)
  }

  # ============================================================================
  # Phase 3: Parse JSON
  # ============================================================================
  parsed_data <- tryCatch(
    {
      jsonlite::fromJSON(pandoc_json_output, simplifyVector = FALSE)
    },
    error = function(e) {
      cli::cli_abort("Failed to parse Pandoc JSON output.", parent = e, call. = FALSE)
    }
  )

  # ============================================================================
  # Phase 4: Validate JSON Structure
  # ============================================================================
  validation_result <- .check_pandoc_json_structure(parsed_data) # Assumes helper exists
  if (!isTRUE(validation_result)) {
    cli::cli_warn(validation_result)
    return(empty_result_string)
  }
  references_meta_list_c <- parsed_data$meta$references$c
  if (length(references_meta_list_c) == 0) {
    cli::cli_warn("No references found in the Pandoc JSON output (meta$references$c is empty).")
    return(empty_result_string)
  }

  # ============================================================================
  # Phase 5: Extract Data
  # ============================================================================
  metadata_df <- .extract_metadata_from_json(references_meta_list_c)
  formatted_df <- .extract_formatted_strings_from_json(parsed_data$blocks)

  if (nrow(metadata_df) == 0) {
    cli::cli_warn("Failed to extract any valid metadata.")
    return(empty_result_string)
  }
  if (nrow(formatted_df) == 0) {
    cli::cli_warn("Could not extract any formatted citation strings from Pandoc JSON 'blocks'.")
    return(empty_result_string)
  }

  # ============================================================================
  # Phase 6: Combine, Label, Highlight (using Helper)
  # ============================================================================
  combined_data <- .process_combined_pub_data(
    metadata_df = metadata_df,
    formatted_df = formatted_df,
    group_labels = group_labels,
    default_label = default_label,
    author_name = author_name,
    author_highlight_markup = author_highlight_markup
  )

  # Add Guard Clause here based on the result of the helper
  if (nrow(combined_data) == 0) {
    # Warning was already issued inside the helper
    return(empty_result_string)
  }

  # ============================================================================
  # Phase 7: Sort Data
  # ============================================================================
  combined_data_sorted <- .sort_publication_data(
    combined_data = combined_data,
    group_order = group_order
  )

  # ============================================================================
  # Phase 8: Generate Typst Output String
  # ============================================================================
  final_typst_block <- .create_typst_output_block(
    combined_data_sorted = combined_data_sorted,
    typst_func_name = typst_func_name
  )

  return(final_typst_block)
}

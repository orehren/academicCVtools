# R/create_publication_list.R

#' Create a grouped, sorted, and formatted publication list for Typst
#'
#' Reads a .bib file, uses Pandoc with a CSL file to generate JSON output
#' containing formatted citations and metadata. Extracts structural information,
#' groups and sorts entries (optionally by custom group order), highlights an
#' author, and returns the result as a Typst code block suitable for a
#' two-column layout (label + formatted item). Argument validation is performed
#' using assertions from the `checkmate` package via an internal helper function.
#'
#' @param bib_file Path to the .bib file. Must be a single, non-empty string
#'        pointing to an existing, readable `.bib` file.
#' @param author_name Name of the author to highlight within the formatted
#'        publication strings. Must be a single, non-empty string.
#' @param csl_file Path to the .csl file. Must be a single, non-empty string
#'        pointing to an existing, readable `.csl` file.
#' @param group_labels Named list or named character vector mapping lowercase
#'        BibTeX entry types (e.g., "article", "inproceedings") to the desired
#'        display labels (e.g., "Journal Article", "Conference Paper").
#'        Both names (BibTeX types) and values (display labels) must be
#'        single, non-empty strings, and names must be unique.
#'        Defaults to a standard set of mappings.
#' @param default_label Label to use for BibTeX types not found in `group_labels`.
#'        Must be a single, non-empty string. Defaults to `"Other"`.
#' @param group_order Optional: A character vector specifying the desired
#'        sort order of the display labels (values from `group_labels` or
#'        `default_label`). Labels present in the data but not in
#'        `group_order` are appended alphabetically after the specified ones.
#'        If provided, must be a character vector with unique, non-missing values.
#'        If `NULL` (default), groups are sorted alphabetically by label.
#' @param pandoc_path Optional: Path to the Pandoc executable. If `NULL` (default),
#'        Pandoc is expected to be in the system's PATH. If provided, must be a
#'        single, non-empty string (existence is checked later).
#' @param author_highlight_markup Typst markup string used for highlighting the
#'        `author_name`. Must contain the placeholder `%s` exactly once, which
#'        will be replaced by the `author_name`. Must be a single, non-empty string.
#'        Defaults to `"#strong[%s]"`.
#' @param typst_func_name Name of the Typst function that will receive the
#'        publication list data (the array of label/item pairs). The output
#'        string will be formatted like `#function_name(( (label:"...", item:"..."), ... ))`.
#'        Must be a single, non-empty string (without the leading '#').
#'        Defaults to `"publication-list"`.
#'
#' @return A single character string containing a Typst code block (````{=typst} ... ````)
#'         with the formatted publication list, ready to be included in a Quarto
#'         document via `output: asis`. Returns an empty Typst array block if
#'         no processable entries are found after validation and processing.
#'
#' @importFrom dplyr %>% select bind_rows
#' @importFrom stringr str_glue str_detect str_remove str_c str_extract
#' @importFrom purrr map_chr map discard set_names map_lgl list_flatten keep safely pluck
#' @importFrom tibble tibble enframe
#' @importFrom jsonlite fromJSON
#' @importFrom rlang check_installed is_named is_list exec sym %||%
#' @importFrom magrittr %>%
#' @importFrom tools file_ext
#' @importFrom stats setNames
#' @importFrom cli cli_abort cli_warn cli_inform
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # --- Setup ---
#' # Create dummy bib file
#' bib_content <- c(
#'   "@article{Author2023Title, author={First Author and Second Author}, title={A Title}, year={2023}, journal={Some Journal}}",
#'   "@inproceedings{Author2022Conf, author={Second Author}, title={Conf Paper}, year={2022}, booktitle={Proc. Conf}}",
#'   "@misc{AuthorNodatePrep, author={First Author}, title={In Prep}, howpublished={In Review}}",
#'   "@book{Third2021Book, author={Third Person}, title={A Book}, year={2021}, publisher={Pub Co}}"
#' )
#' bib_file <- tempfile(fileext = ".bib")
#' writeLines(bib_content, bib_file)
#'
#' # Create dummy CSL (or use a real one) - e.g., download apa.csl
#' # For this example, create a minimal dummy file if needed
#' csl_content <- '<?xml version="1.0" encoding="utf-8"?>
#' <style xmlns="http://purl.org/net/xbiblio/csl" class="in-text" version="1.0">
#'   <info>
#'     <title>Minimal Example</title>
#'     <id>minimal-example</id>
#'     <link href="http://example.com" rel="self"/>
#'     <updated>2024-01-01T00:00:00+00:00</updated>
#'   </info>
#'   <bibliography>
#'     <layout>
#'       <text variable="title"/>
#'     </layout>
#'   </bibliography>
#' </style>'
#' csl_file <- tempfile(fileext = ".csl")
#' writeLines(csl_content, csl_file)
#'
#' author_to_highlight <- "First Author"
#'
#' # --- Basic Usage ---
#' # Assumes pandoc is in PATH
#' try(
#'   {
#'     typst_out_basic <- create_publication_list(
#'       bib_file = bib_file,
#'       author_name = author_to_highlight,
#'       csl_file = csl_file
#'     )
#'     cat(typst_out_basic)
#'   },
#'   silent = TRUE
#' )
#'
#' # --- Custom Grouping and Order ---
#' custom_labels <- c(article = "Peer-Reviewed", book = "Monographs", misc = "Other")
#' custom_order <- c("Peer-Reviewed", "Monographs", "Conference Items", "Other")
#' # Note: "Conference Items" comes from default_label mapping for inproceedings
#'
#' try(
#'   {
#'     typst_out_custom <- create_publication_list(
#'       bib_file = bib_file,
#'       author_name = author_to_highlight,
#'       csl_file = csl_file,
#'       group_labels = custom_labels,
#'       default_label = "Conference Items", # Assign remaining types here
#'       group_order = custom_order,
#'       author_highlight_markup = "#emph[%s]"
#'     )
#'     cat(typst_out_custom)
#'   },
#'   silent = TRUE
#' )
#'
#' # --- Invalid Input Example (should error) ---
#' try(
#'   {
#'     create_publication_list(
#'       bib_file = "nonexistent.bib", # File doesn't exist
#'       author_name = author_to_highlight,
#'       csl_file = csl_file
#'     )
#'   },
#'   error = function(e) {
#'     print(paste("Successfully caught expected error:", e$message))
#'   }
#' )
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

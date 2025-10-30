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
#' 3.  **JSON Parsing & Transformation:** Parses the JSON and transforms it into a
#'     clean data frame. This includes handling empty/invalid inputs, extracting
#'     metadata, formatting citation strings, grouping, and author highlighting.
#' 4.  **Typst Output Generation:** Generates the final Typst code block.
#'
#' @param bib_file Path to the `.bib` bibliography file.
#' @param author_name The full name of the author to highlight in publications.
#' @param csl_file Path to the `.csl` citation style file for formatting.
#' @param group_labels A named character vector to map BibTeX entry types
#'   (e.g., `article`) to user-friendly display labels (e.g., `"Journal Article"`).
#' @param default_label The display label for any entry types not found in
#'   `group_labels`.
#' @param group_order An optional character vector specifying the desired
#'   display order of the `group_labels`.
#' @param author_highlight_markup The Typst markup to apply for highlighting the
#'   author.
#' @param typst_func_name The name of the Typst function that will receive the
#'   publication data.
#' @param pandoc_path An optional path to the Pandoc executable.
#'
#' @return A single character string containing a Typst code block (` ```{=typst} ... ``` `).
#'   If no valid publication entries are found, it returns an empty Typst array block.
#'   This output is intended to be used in a Quarto document with `output: asis`.
#'
#' @export
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
  # --- Phase 0: Validate Arguments ---
  .validate_create_publication_list_args(
    bib_file, author_name, csl_file, group_labels, default_label,
    group_order, pandoc_path, author_highlight_markup, typst_func_name
  )

  # --- Phase 1: Setup ---
  pandoc_cmd <- .validate_executable_found("pandoc", pandoc_path, "pandoc_path")
  empty_result_string <- sprintf("```{=typst}\n#%s(())\n```", typst_func_name)

  # --- Phase 2: Call Pandoc ---
  pandoc_json_output <- .call_pandoc_json(bib_file, csl_file, pandoc_cmd)
  if (is.null(pandoc_json_output)) return(empty_result_string)

  # --- Phase 3: Parse and Transform Data ---
  parsed_data <- tryCatch(
    jsonlite::fromJSON(pandoc_json_output, simplifyVector = FALSE),
    error = function(e) {
      cli::cli_abort("Failed to parse Pandoc JSON output.", parent = e)
    }
  )

  references <- purrr::pluck(parsed_data, "meta", "references", "c")
  blocks <- purrr::pluck(parsed_data, "blocks", 1) # Note the change here

  if (is.null(references) || is.null(blocks) || length(references) == 0) {
    cli::cli_warn("Pandoc JSON is missing data or contains no references.")
    return(empty_result_string)
  }

  metadata_df <- purrr::map_dfr(references, function(x) {
    tibble::tibble(
      key = purrr::pluck(x, "c", "id", "c", 1, .default = NA_character_),
      bibtype = purrr::pluck(x, "c", "type", "c", 1, .default = "misc"),
      year = purrr::pluck(x, "c", "issued", "c", 1, .default = NA_character_) %>%
        stringr::str_extract("\\d{4}") %>%
        as.integer()
    )
  }) %>%
    dplyr::mutate(
      bibtype = dplyr::recode(
        .data$bibtype,
        "article-journal" = "article",
        "paper-conference" = "inproceedings",
        "chapter" = "incollection",
        "thesis" = "phdthesis",
        "report" = "techreport",
        "manuscript" = "unpublished",
        .default = .data$bibtype
      )
    )

  formatted_strings_df <- .extract_formatted_strings_from_ast(blocks)

  publication_data <- metadata_df %>%
    dplyr::left_join(formatted_strings_df, by = "key") %>%
    dplyr::filter(!is.na(.data$formatted_string), nzchar(.data$formatted_string)) %>%
    dplyr::mutate(
      label = dplyr::recode(.data$bibtype, !!!group_labels, .default = default_label),
      formatted_string = stringr::str_replace_all(
        .data$formatted_string,
        pattern = stringr::fixed(author_name),
        replacement = sprintf(author_highlight_markup, author_name)
      )
    )

  # --- Phase 4: Sort Data ---
  if (!is.null(group_order)) {
    all_labels <- unique(publication_data$label)
    ordered_labels <- intersect(group_order, all_labels)
    other_labels <- sort(setdiff(all_labels, ordered_labels))
    final_levels <- c(ordered_labels, other_labels)
    publication_data <- publication_data %>%
      dplyr::mutate(label = factor(.data$label, levels = final_levels))
  }
  publication_data <- publication_data %>%
    dplyr::arrange(.data$label, dplyr::desc(.data$year)) %>%
    dplyr::mutate(label = as.character(.data$label)) # Convert back to character

  # --- Phase 5: Generate Typst Output ---
  .create_typst_output_block(publication_data, typst_func_name)
}

#' Generate the final Typst output block for the publication list
#'
#' @param combined_data_sorted The final, sorted tibble.
#' @param typst_func_name The name of the Typst function.
#'
#' @return A single character string containing the Typst code block.
#' @noRd
.create_typst_output_block <- function(combined_data_sorted, typst_func_name) {
  empty_result_string <- sprintf("```{=typst}\n#%s(())\n```", typst_func_name)
  if (nrow(combined_data_sorted) == 0) return(empty_result_string)

  typst_entry_strings <- purrr::map_chr(1:nrow(combined_data_sorted), ~ {
    current_entry <- combined_data_sorted[.x, ]
    escaped_label <- .escape_typst_string(as.character(current_entry$label)) # Ensure character
    item_for_r_string <- gsub("\\", "\\\\", current_entry$formatted_string, fixed = TRUE)
    item_for_r_string <- gsub('"', '\\"', item_for_r_string, fixed = TRUE)
    sprintf('  (label: "%s", item: "%s")', escaped_label, item_for_r_string)
  })

  typst_body <- paste(typst_entry_strings, collapse = ",\n")
  sprintf("```{=typst}\n#%s((\n%s\n))\n```", typst_func_name, typst_body)
}

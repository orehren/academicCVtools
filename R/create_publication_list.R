# R/create_publication_list.R

#' Create a Formatted Publication List for Typst
#'
#' Converts a `.bib` file into a Typst-ready string, perfect for an academic CV.
#'
#' @details
#' This function automates the process of creating a clean, grouped, and sorted
#' publication list for use in Typst and Quarto documents. It's designed for
#' academics who want a reproducible, data-driven CV.
#'
#' Here's how it works:
#' 1.  **Reads Your Bibliography:** Takes your `.bib` file as input.
#' 2.  **Formats with Pandoc:** Uses Pandoc and a CSL style file to format your
#'     citations into a structured format (JSON).
#' 3.  **Processes and Transforms:** Cleans up the data, groups your publications
#'     (e.g., "Journal Article", "Book Chapter"), highlights your name, and
#'     sorts everything by group and year.
#' 4.  **Generates Typst Code:** Produces a clean Typst code block that calls a
#'     Typst function with your publication data, ready to be included in your
#'     CV.
#'
#' @param bib_file Path to your `.bib` bibliography file.
#' @param author_name Your full name, exactly as you want it highlighted in the
#'   publication list.
#' @param csl_file Path to a `.csl` citation style file for formatting your
#'   references.
#' @param group_labels A named vector that maps the BibTeX entry types (like
#'   `"article"` or `"inproceedings"`) to the friendly group titles you want in
#'   your CV (like `"Journal Articles"` or `"Conference Papers"`).
#' @param default_label The label to use for any publication type not listed in
#'   `group_labels`. Defaults to `"Other"`.
#' @param group_order A vector of the group titles from `group_labels` in the
#'   exact order you want them to appear in your CV. Any groups not listed here
#'   will be added at the end, sorted alphabetically.
#' @param author_highlight_markup The Typst code used to highlight your name.
#'   Use `%s` as a placeholder for the `author_name`. Defaults to `"#strong[%s]"`
#'   (bold).
#' @param typst_func_name The name of the Typst function that will display your
#'   publication list. Defaults to `"publication-list"`.
#' @param pandoc_path Optional: The full path to the Pandoc executable. If `NULL`
#'   (the default), the function will automatically look for Pandoc in your
#'   system's PATH.
#'
#' @return A single character string containing a Typst code block (` ```{=typst} ... ``` `).
#'   If no valid publication entries are found, it returns an empty Typst array block.
#'   This output is intended to be used in a Quarto document with `output: asis`.
#'
#' @importFrom dplyr filter mutate recode left_join desc arrange
#' @importFrom purrr pluck map_dfr
#' @importFrom stringr str_replace_all
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
#' @examples
#' # This function requires Pandoc to be installed and available in the system's PATH.
#' # We will only run the example if Pandoc is found.
#' if (academicCVtools:::.is_pandoc_available()) {
#'
#' # --- 1. Basic Example ---
#' # Create dummy files for a reproducible example
#' bib_content <- c(
#'   "@article{Doe2023,
#'     author  = {Jane Doe and John Smith},
#'     title   = {A Brilliant Paper on R},
#'     journal = {Journal of R Studies},
#'     year    = {2023}
#'   }",
#'   "@inproceedings{Doe2022,
#'     author  = {Jane Doe},
#'     title   = {A Talk on Tidyverse},
#'     booktitle = {Proceedings of the R Conference},
#'     year    = {2022}
#'   }"
#' )
#' bib_file <- tempfile(fileext = ".bib")
#' writeLines(bib_content, bib_file)
#'
#' csl_content <- '<?xml version="1.0" encoding="utf-8"?>
#' <style xmlns="http://purl.org/net/xbiblio/csl" class="in-text" version="1.0">
#'   <info>
#'     <title>Simple APA-like Style</title>
#'     <id>simple-apa</id>
#'   </info>
#'   <bibliography>
#'     <layout>
#'       <text variable="author" suffix=". "/>
#'       <text variable="title" text-case="title" suffix=". "/>
#'       <text variable="container-title" font-style="italic" suffix=", "/>
#'       <text variable="volume"/>
#'       <text variable="page" prefix="(" suffix=")"/>
#'       <text variable="issued" prefix=" (" suffix=")."/>
#'     </layout>
#'   </bibliography>
#' </style>'
#' csl_file <- tempfile(fileext = ".csl")
#' writeLines(csl_content, csl_file)
#'
#' # Generate the Typst output, highlighting "Jane Doe"
#' typst_output <- create_publication_list(
#'   bib_file = bib_file,
#'   author_name = "Jane Doe",
#'   csl_file = csl_file
#' )
#' cat(typst_output)
#'
#' # --- 2. Custom Grouping and Order ---
#' # Define custom labels and a specific display order
#' custom_labels <- c(
#'   article = "Peer-Reviewed Journal Articles",
#'   inproceedings = "Conference Presentations"
#' )
#' custom_order <- c(
#'   "Peer-Reviewed Journal Articles",
#'   "Conference Presentations"
#' )
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
#' # --- 3. Clean up the temporary files ---
#' unlink(bib_file)
#' unlink(csl_file)
#'
#' } else {
#'   cat("Pandoc is not available, skipping example.\n")
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
  blocks <- purrr::pluck(parsed_data, "blocks", 1)

  if (is.null(references)) {
    cli::cli_warn("Pandoc JSON appears to be missing reference metadata (`meta$references`).")
    return(empty_result_string)
  }
  if (is.null(blocks)) {
    cli::cli_warn("Pandoc JSON appears to be missing formatted content (`blocks`).")
    return(empty_result_string)
  }
  if (length(references) == 0) {
    cli::cli_warn("The bibliography file appears to be empty; no references found.")
    return(empty_result_string)
  }

  metadata_df <- purrr::map_dfr(references, .parse_single_reference) |>
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

  publication_data <- metadata_df |>
    dplyr::left_join(formatted_strings_df, by = "key") |>
    dplyr::filter(!is.na(.data$formatted_string), nzchar(.data$formatted_string)) |>
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
    present_labels <- unique(publication_data$label)
    ordered_labels <- intersect(group_order, present_labels)
    remaining_labels <- setdiff(present_labels, group_order) |> sort()
    final_levels <- c(ordered_labels, remaining_labels)

    publication_data <- publication_data |>
      dplyr::mutate(label = factor(.data$label, levels = final_levels))
  }

  publication_data <- publication_data |>
    dplyr::arrange(.data$label, dplyr::desc(.data$year)) |>
    dplyr::mutate(label = as.character(.data$label))

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
    escaped_label <- current_entry$label |> as.character() |> .escape_typst_string()
    item_for_r_string <- gsub("\\", "\\\\", current_entry$formatted_string, fixed = TRUE)
    item_for_r_string <- gsub('"', '\\"', item_for_r_string, fixed = TRUE)
    sprintf('  (label: "%s", item: "%s")', escaped_label, item_for_r_string)
  })

  typst_body <- paste(typst_entry_strings, collapse = ",\n")
  sprintf("```{=typst}\n#%s((\n%s\n))\n```", typst_func_name, typst_body)
}

# R/create_publication_list_helpers.R - Internal Helpers for Publication List

#' Call Pandoc and return JSON output as a single string
#'
#' Executes Pandoc using the `processx` package for robust process management.
#'
#' @param bib_file Path to the BibTeX file.
#' @param csl_file Path to the CSL file.
#' @param pandoc_cmd The command/path for the pandoc executable.
#'
#' @return A single character string containing the JSON output from Pandoc.
#'         Returns `NULL` if Pandoc fails or produces empty output.
#'
#' @importFrom processx run
#' @importFrom cli cli_abort cli_warn
#' @noRd
.call_pandoc_json <- function(bib_file, csl_file, pandoc_cmd) {
  args <- c(bib_file, "--csl", csl_file, "--citeproc", "-t", "json")

  # For more informative error messages
  full_command_str <- paste(pandoc_cmd, paste(shQuote(args), collapse = " "))

  result <- processx::run(
    command = pandoc_cmd,
    args = args,
    error_on_status = FALSE
  )

  if (result$status != 0) {
    cli::cli_warn(
      c(
        "x" = "Pandoc command failed with status code {result$status}.",
        "i" = "Command: {.code {full_command_str}}",
        "i" = "Stderr: {result$stderr}"
      )
    )
    return(NULL)
  }

  if (nchar(result$stdout) == 0) {
    cli::cli_warn("Pandoc produced empty JSON output.")
    return(NULL)
  }
  return(result$stdout)
}

#' Parse a list of inline nodes from the Pandoc AST
#'
#' This helper function takes a list of Pandoc's inline nodes (like 'Str' for
#' string and 'Space') and reconstructs the original formatted text.
#'
#' @param inline_nodes A list of inline nodes from a "Para" block.
#' @return A single character string concatenating the text content.
#' @noRd
.parse_ast_inline_nodes <- function(inline_nodes) {
  purrr::map_chr(inline_nodes, function(node) {
    node_type <- purrr::pluck(node, "t")
    if (identical(node_type, "Str")) return(purrr::pluck(node, "c"))
    if (identical(node_type, "Space")) return(" ")
    "" # Ignore other node types like 'Cite', 'Emph', etc.
  }) |>
    stringr::str_c(collapse = "")
}

#' Parse a single entry div from the Pandoc AST
#'
#' A 'div' in Pandoc's AST represents a single formatted bibliography entry.
#' This function extracts the citation key and the full formatted text for one entry.
#'
#' @param entry_div A single 'div' list from the main list of entries.
#' @return A one-row tibble with `key` and `formatted_string`.
#' @noRd
.parse_ast_entry_div <- function(entry_div) {
  # Path to citation key: div > "c" > attributes > id
  key <- purrr::pluck(entry_div, "c", 1, 1, .default = "") |>
    stringr::str_remove("^ref-")

  # Path to content: div > "c" > content blocks (a list of "Para" nodes)
  content_blocks <- purrr::pluck(entry_div, "c", 2)

  formatted_string <- purrr::map_chr(content_blocks, function(para_node) {
    # Path to inline content: Para > "c" > list of inline nodes
    inline_nodes <- purrr::pluck(para_node, "c")
    .parse_ast_inline_nodes(inline_nodes)
  }) |>
    stringr::str_c(collapse = "\n")

  tibble::tibble(key = key, formatted_string = formatted_string)
}

#' Extract formatted citation strings from Pandoc's JSON AST
#'
#' This function traverses the nested list structure of Pandoc's JSON output
#' to piece together the formatted citation string for each reference. It does
#' this by mapping over the list of entry 'divs' and parsing each one.
#'
#' @param blocks The 'blocks' list from the parsed Pandoc JSON.
#' @return A tibble with 'key' and 'formatted_string' columns.
#' @importFrom purrr map_dfr pluck
#' @importFrom stringr str_remove str_c
#' @importFrom tibble tibble
#' @noRd
.extract_formatted_strings_from_ast <- function(blocks) {
  # The formatted entries are in a list of 'divs'.
  # Path to the list: blocks > "c" (content) > 2nd element (the Div) > "c" (content)
  entry_divs <- purrr::pluck(blocks, "c", 2)

  if (is.null(entry_divs)) {
    return(tibble::tibble(key = character(), formatted_string = character()))
  }

  purrr::map_dfr(entry_divs, .parse_ast_entry_div)
}

#' Parse a single reference from Pandoc's JSON metadata
#'
#' @param ref A single reference list object.
#' @return A one-row tibble with `key`, `bibtype`, and `year`.
#' @noRd
.parse_single_reference <- function(ref) {
  tibble::tibble(
    # Path to citation key: ref > "c" > "id" > "c" > value
    key = purrr::pluck(ref, "c", "id", "c", 1, .default = NA_character_),
    # Path to entry type: ref > "c" > "type" > "c" > value
    bibtype = purrr::pluck(ref, "c", "type", "c", 1, .default = "misc"),
    # Path to year: ref > "c" > "issued" > "c" > date parts > year
    year = purrr::pluck(ref, "c", "issued", "c", 1, .default = NA_character_) |>
      stringr::str_extract("\\d{4}") |>
      as.integer()
  )
}

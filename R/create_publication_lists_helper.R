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
  result <- processx::run(
    command = pandoc_cmd,
    args = args,
    error_on_status = FALSE
  )

  if (result$status != 0) {
    cli::cli_warn(c("Pandoc failed with status code {result$status}.", "i" = "Stderr: {result$stderr}"))
    return(NULL)
  }

  if (nchar(result$stdout) == 0) {
    cli::cli_warn("Pandoc produced empty JSON output.")
    return(NULL)
  }
  return(result$stdout)
}

#' Extract formatted citation strings from Pandoc's JSON AST
#'
#' This function traverses the nested list structure of Pandoc's JSON output
#' to piece together the formatted citation string for each reference.
#'
#' @param blocks The 'blocks' list from the parsed Pandoc JSON.
#' @return A tibble with 'key' and 'formatted_string' columns.
#' @importFrom purrr map_dfr map_chr pluck
#' @importFrom stringr str_remove str_c
#' @importFrom tibble tibble
#' @noRd
.extract_formatted_strings_from_ast <- function(blocks) {
  entry_divs <- purrr::pluck(blocks, "c", 2)
  if (is.null(entry_divs)) return(tibble::tibble(key = character(), formatted_string = character()))

  purrr::map_dfr(entry_divs, ~{
    key <- purrr::pluck(.x, "c", 1, 1, .default = "") %>%
      stringr::str_remove("^ref-")

    content_parts <- purrr::map_chr(purrr::pluck(.x, "c", 2), function(para_node) {
      if (!identical(purrr::pluck(para_node, "t"), "Para")) return("")
      inline_parts <- purrr::map_chr(purrr::pluck(para_node, "c"), function(inline_node) {
        if (identical(purrr::pluck(inline_node, "t"), "Str")) return(purrr::pluck(inline_node, "c"))
        if (identical(purrr::pluck(inline_node, "t"), "Space")) return(" ")
        ""
      })
      stringr::str_c(inline_parts, collapse = "")
    })

    formatted_string <- stringr::str_c(content_parts, collapse = "\n")
    tibble::tibble(key = key, formatted_string = formatted_string)
  })
}

#' Parse a single reference from Pandoc's JSON metadata
#'
#' @param ref A single reference list object.
#' @return A one-row tibble with `key`, `bibtype`, and `year`.
#' @noRd
.parse_single_reference <- function(ref) {
  tibble::tibble(
    key = purrr::pluck(ref, "c", "id", "c", 1, .default = NA_character_),
    bibtype = purrr::pluck(ref, "c", "type", "c", 1, .default = "misc"),
    year = purrr::pluck(ref, "c", "issued", "c", 1, .default = NA_character_) %>%
      stringr::str_extract("\\d{4}") %>%
      as.integer()
  )
}

# R/create_publication_list_validators.R

#' Validate the structure of parsed Pandoc JSON
#'
#' Checks if the parsed JSON from Pandoc contains the expected top-level
#' keys (`meta`, `blocks`) and nested keys required for data extraction.
#'
#' @param parsed_json The parsed JSON object (a list).
#'
#' @return `TRUE` if the structure is valid, otherwise a character string
#'         with a descriptive warning message.
#' @noRd
.validate_pandoc_json_structure <- function(parsed_json) {
  # Check for top-level keys
  if (!all(c("meta", "blocks") %in% names(parsed_json))) {
    return("Invalid Pandoc JSON structure: Top-level keys (`meta`, `blocks`) check failed.")
  }

  # Check for nested keys in 'meta'
  if (!"references" %in% names(parsed_json$meta) || !"c" %in% names(parsed_json$meta$references)) {
    return("Invalid Pandoc JSON structure: `meta$references$c` not found.")
  }

  # Check for 'c' and 't' keys inside 'blocks'
  if (!"c" %in% names(parsed_json$blocks) || !"t" %in% names(parsed_json$blocks)) {
    return("Invalid Pandoc JSON structure: `blocks` is missing 'c' or 't' keys.")
  }

  # If all checks pass, return TRUE
  TRUE
}

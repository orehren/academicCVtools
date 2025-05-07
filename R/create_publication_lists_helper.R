# R/create_publication_list_helpers.R - Internal Helpers for Publication List

#' Call Pandoc and return JSON output as a single string
#' Uses the bib file directly as input for citeproc.
#' @importFrom cli cli_abort cli_inform cli_warn
#' @importFrom checkmate test_list test_subset
#' @importFrom dplyr bind_rows recode
#' @importFrom purrr map discard map_chr pluck safely
#' @importFrom rlang .data is_list %||%
#' @importFrom stringr str_extract str_detect str_remove str_c
#' @importFrom tibble tibble enframe
#'
#'
#' @noRd
.call_pandoc_json <- function(bib_file, csl_file, pandoc_cmd) {
  bib_file_norm <- tryCatch(normalizePath(bib_file, mustWork = FALSE), warning = function(w) bib_file)
  csl_file_norm <- tryCatch(normalizePath(csl_file, mustWork = FALSE), warning = function(w) csl_file)
  if (!file.exists(bib_file)) {
    cli::cli_abort("BibTeX file not found: {.path {bib_file_norm}}", call. = FALSE)
  }
  if (!file.exists(csl_file)) {
    cli::cli_abort("CSL file not found: {.path {csl_file_norm}}", call. = FALSE)
  }
  args <- c(bib_file, "--csl", csl_file, "--citeproc", "-t", "json")
  stdout_res <- NULL
  stderr_res <- character(0)
  status_code <- -1
  temp_stdout_file <- tempfile(pattern = "pandoc_stdout_", fileext = ".json")
  temp_stderr_file <- tempfile(pattern = "pandoc_stderr_", fileext = ".txt")
  on.exit(
    {
      if (file.exists(temp_stdout_file)) unlink(temp_stdout_file)
      if (file.exists(temp_stderr_file)) unlink(temp_stderr_file)
    },
    add = TRUE
  )
  tryCatch(
    {
      status_code <- system2(command = pandoc_cmd, args = args, stdout = temp_stdout_file, stderr = temp_stderr_file, wait = TRUE)
    },
    error = function(e) {
      stderr_content <- character(0)
      if (file.exists(temp_stderr_file) && file.info(temp_stderr_file)$size > 0) {
        stderr_content <- readLines(temp_stderr_file, warn = FALSE)
      }
      cli::cli_abort(message = c("Error executing Pandoc command.", "x" = conditionMessage(e), "!" = if (length(stderr_content) > 0) paste(stderr_content, collapse = "\n") else "No stderr output."), call. = FALSE)
    }
  )
  stderr_output <- character(0)
  if (file.exists(temp_stderr_file) && file.info(temp_stderr_file)$size > 0) {
    stderr_output <- readLines(temp_stderr_file, warn = FALSE)
    if (any(nzchar(trimws(stderr_output)))) {
      cli::cli_inform("Pandoc stderr output:\n{paste(stderr_output, collapse = '\\n')}")
    }
  }
  stdout_output <- NULL
  if (file.exists(temp_stdout_file) && file.info(temp_stdout_file)$size > 0) {
    stdout_output <- readChar(temp_stdout_file, file.info(temp_stdout_file)$size, useBytes = TRUE)
  }
  if (status_code != 0 || is.null(stdout_output) || nchar(stdout_output) == 0) {
    cli::cli_warn("Pandoc failed (status code: {status_code}) or produced empty JSON output.")
    return(NULL)
  }
  return(stdout_output)
}

# --- JSON Structure Validation ---

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

# --- Metadata Extraction ---

#' Process metadata for a single reference item (MetaMap)
#' Extracts key, bibtype, and year assuming 'issued' is a MetaString.
#' Uses simplified guard clause.
#' @param item A list representing a single reference MetaMap object.
#' @return A 1-row tibble with key, bibtype, year, or NULL if invalid/incomplete.
#' @noRd
.process_single_reference_meta <- function(item) {
  item_t <- purrr::pluck(item, "t", .default = NULL)
  item_c_list <- purrr::pluck(item, "c", .default = NULL)
  is_valid_metamap <- identical(item_t, "MetaMap") && rlang::is_list(item_c_list)
  if (!is_valid_metamap) {
    return(NULL)
  }
  item_content <- item_c_list
  key <- .safe_get_value(item_content, path = c("id", "c"), default = NA_character_)
  is_valid_key <- !is.na(key) && nzchar(key)
  if (!is_valid_key) {
    return(NULL)
  }
  csl_type <- .safe_get_value(item_content, path = c("type", "c"), default = "misc")
  if (csl_type == "") csl_type <- "misc"
  year_val <- NA_character_
  issued_string <- .safe_get_value(item_content, path = c("issued", "c"), default = NA_character_)
  if (!is.na(issued_string)) {
    year_match <- stringr::str_extract(issued_string, "\\b\\d{4}\\b")
    if (!is.na(year_match)) {
      year_val <- year_match
    }
  }
  year_int <- as.integer(year_val %||% "0")
  bibtype <- dplyr::recode(csl_type, "article-journal" = "article", "paper-conference" = "inproceedings", "chapter" = "incollection", "book" = "book", "thesis" = "phdthesis", "report" = "techreport", "manuscript" = "unpublished", .default = "misc")
  return(tibble::tibble(key = key, bibtype = bibtype, year = year_int))
}

#' Extract metadata from parsed Pandoc JSON references by processing each item
#' @noRd
.extract_metadata_from_json <- function(references_meta_list_c) {
  metadata_rows <- purrr::map(references_meta_list_c, .process_single_reference_meta)
  valid_rows <- purrr::discard(metadata_rows, is.null)
  if (length(valid_rows) == 0) {
    return(tibble::tibble(key = character(), bibtype = character(), year = integer()))
  }
  return(dplyr::bind_rows(valid_rows))
}

# --- Formatted String Extraction ---

#' Process a single entry Div from Pandoc JSON blocks
#' Extracts key and renders the formatted string from the AST.
#' Uses guard clauses to avoid deep nesting.
#' @param entry_div A list representing a single entry Div.
#' @return A list containing 'key' and 'formatted_string' or NULL if invalid.
#' @noRd
.process_single_entry_div <- function(entry_div) {
  if (!rlang::is_list(entry_div) || length(entry_div) != 2 || is.null(entry_div$t) || !identical(entry_div$t, "Div") || !rlang::is_list(entry_div$c) || length(entry_div$c) != 2) {
    return(NULL)
  }
  id_structure <- entry_div$c[[1]]
  if (!rlang::is_list(id_structure) || length(id_structure) < 1) {
    return(NULL)
  }
  div_id <- id_structure[[1]]
  if (!is.character(div_id) || length(div_id) != 1 || !stringr::str_detect(div_id, "^ref-")) {
    return(NULL)
  }
  key <- stringr::str_remove(div_id, "^ref-")
  content_blocks <- entry_div$c[[2]]
  if (!rlang::is_list(content_blocks)) {
    full_formatted_string <- ""
  } else {
    full_formatted_string <- stringr::str_c(purrr::map_chr(content_blocks, .render_ast_node_to_string), collapse = "\n")
  } # Assumes .render_ast_node_to_string is available
  if (!nzchar(key) || !nzchar(full_formatted_string)) {
    return(NULL)
  }
  return(list(key = key, formatted_string = trimws(full_formatted_string)))
}

#' Extract formatted strings from parsed Pandoc JSON blocks by calling helper
#' @noRd
.extract_formatted_strings_from_json <- function(blocks) {
  formatted_strings_list <- list()
  if (!rlang::is_list(blocks) || length(blocks) == 0) {
    cli::cli_warn("Pandoc JSON 'blocks' element is not a list or is empty.")
    return(tibble::tibble(key = character(), formatted_string = character()))
  }
  top_level_div <- blocks[[1]]
  if (!rlang::is_list(top_level_div) || is.null(top_level_div$t) || top_level_div$t != "Div" || !rlang::is_list(top_level_div$c) || length(top_level_div$c) < 2 || !rlang::is_list(top_level_div$c[[2]])) {
    cli::cli_warn("Unexpected structure within the main block element (blocks[[1]]). Cannot find entry list.")
    return(tibble::tibble(key = character(), formatted_string = character()))
  }
  entry_divs <- top_level_div$c[[2]]
  results <- purrr::map(entry_divs, .process_single_entry_div)
  valid_results <- purrr::discard(results, is.null)
  if (length(valid_results) == 0) {
    return(tibble::tibble(key = character(), formatted_string = character()))
  }
  keys <- purrr::map_chr(valid_results, "key")
  values <- purrr::map_chr(valid_results, "formatted_string")
  formatted_df <- tibble::tibble(key = keys, formatted_string = values)
  return(formatted_df)
}


#' Combine, label, and highlight publication data
#'
#' Joins metadata and formatted strings, filters invalid entries,
#' applies group labels, and highlights the specified author.
#' Assumes input data frames are valid.
#'
#' @param metadata_df Tibble with columns 'key', 'bibtype', 'year'.
#' @param formatted_df Tibble with columns 'key', 'formatted_string'.
#' @param group_labels Named list/vector for label mapping.
#' @param default_label Default label string.
#' @param author_name Author name string to highlight.
#' @param author_highlight_markup Typst markup string with '%s'.
#'
#' @return A processed tibble with 'key', 'bibtype', 'year',
#'         'formatted_string' (highlighted), and 'label' columns,
#'         or a 0-row tibble if no valid entries remain after join/filter.
#'
#' @importFrom dplyr left_join filter mutate recode %>%
#' @importFrom rlang !!! sym
#' @importFrom stringr str_replace_all fixed
#' @importFrom cli cli_warn
#'
#' @noRd
.process_combined_pub_data <- function(metadata_df, formatted_df,
                                       group_labels, default_label,
                                       author_name, author_highlight_markup) {

  # --- 1. Combine Data ---
  combined_data <- dplyr::left_join(metadata_df, formatted_df, by = "key")

  # --- 2. Filter Invalid/Incomplete Entries ---
  # Filter entries where join failed or essential data is missing
  combined_data <- combined_data %>%
    dplyr::filter(!is.na(.data$bibtype) & !is.na(.data$formatted_string) & nzchar(.data$formatted_string))
  # Note: We keep entries with year = 0L (originally NA) based on previous decision

  # Guard Clause: Check if any data remains after join/filter
  if (nrow(combined_data) == 0) {
    cli::cli_warn("No entries remaining after joining metadata and formatted strings.")
    # Return the empty structure expected by later steps
    return(tibble::tibble(
      key = character(),
      bibtype = character(),
      year = integer(),
      formatted_string = character(),
      label = character() # Add label column definition
    ))
  }

  # --- 3. Add Labels ---
  combined_data <- combined_data %>%
    dplyr::mutate(
      # Use !!! to splice the named list/vector group_labels into recode
      label = dplyr::recode(.data$bibtype, !!!group_labels, .default = default_label)
    )

  # --- 4. Highlight Author ---
  # Prepare replacement string once
  replacement_string <- sprintf(author_highlight_markup, author_name)
  combined_data <- combined_data %>%
    dplyr::mutate(
      # Apply replacement only to non-NA formatted strings (already filtered NAs)
      formatted_string = stringr::str_replace_all(
        .data$formatted_string,
        pattern = stringr::fixed(author_name),
        replacement = replacement_string
      )
    )

  return(combined_data)
}


#' Sort processed publication data
#'
#' Sorts the combined data frame first by group label (respecting custom
#' `group_order` if provided, otherwise alphabetically) and then by year
#' descending. Converts the label column back to character after sorting.
#' Assumes input data frame has 'label' and 'year' columns.
#'
#' @param combined_data The tibble produced by `.process_combined_pub_data`.
#' @param group_order Optional character vector for custom sort order of labels.
#'
#' @return The sorted tibble with the 'label' column as character.
#'
#' @importFrom dplyr mutate arrange desc %>%
#' @importFrom cli cli_inform
#'
#' @noRd
.sort_publication_data <- function(combined_data, group_order) {

  # Guard clause: If data is empty, return it as is
  if (nrow(combined_data) == 0) {
    return(combined_data)
  }

  data_to_sort <- combined_data # Work with a copy or directly? Let's use original.

  # --- Apply custom group order if provided ---
  if (!is.null(group_order) && length(group_order) > 0) {

    # Get unique labels present in the actual data
    all_data_labels <- unique(data_to_sort$label)

    # Determine the final factor levels based on group_order and data labels
    ordered_labels_present <- intersect(group_order, all_data_labels)
    other_labels <- setdiff(all_data_labels, ordered_labels_present)
    # Sort the remaining labels alphabetically for consistent ordering
    sorted_other_labels <- sort(other_labels)
    final_levels <- c(ordered_labels_present, sorted_other_labels)

    # Convert 'label' column to factor with the calculated levels
    data_to_sort <- data_to_sort %>%
      dplyr::mutate(label = factor(.data$label, levels = final_levels))

    cli::cli_inform("Sorting groups by custom order: {paste(final_levels, collapse=', ')}")
  }
  # If group_order is NULL, 'label' remains character and arrange() sorts alphabetically.

  # --- Perform sorting ---
  # Arrange by label (respects factor levels if set), then year descending
  # year = 0L for NA values will typically sort them first when descending,
  # which might be desired (e.g., "in press" before older years).
  combined_data_sorted <- data_to_sort %>%
    dplyr::arrange(.data$label, dplyr::desc(.data$year))

  # --- Convert label back to character ---
  # Important for consistent output type, regardless of whether factor was used
  if (is.factor(combined_data_sorted$label)) {
    combined_data_sorted <- combined_data_sorted %>%
      dplyr::mutate(label = as.character(.data$label))
  }

  return(combined_data_sorted)
}


#' Generate the final Typst output block for the publication list
#'
#' Iterates over the sorted data frame and creates the Typst array
#' of dictionaries, wrapped in a Typst code block.
#' Assumes input data frame is valid and sorted.
#'
#' @param combined_data_sorted The final, sorted tibble with columns
#'        'label' and 'formatted_string'.
#' @param typst_func_name The name of the Typst function (string, without #).
#'
#' @return A single character string containing the Typst code block.
#'         Returns an empty Typst array block if input data is empty.
#'
#' @importFrom purrr map_chr
#' @importFrom stringr str_c str_glue
#'
#' @noRd
.create_typst_output_block <- function(combined_data_sorted, typst_func_name) {

  # Define the structure for an empty result upfront
  empty_result_string <- sprintf("```{=typst}\n#%s(())\n```", typst_func_name)

  # Guard clause: If data is empty, return the predefined empty block
  if (nrow(combined_data_sorted) == 0) {
    return(empty_result_string)
  }

  # --- Generate Typst dictionary strings for each entry ---
  typst_entry_strings <- purrr::map_chr(1:nrow(combined_data_sorted), ~ {
    current_entry <- combined_data_sorted[.x, ]

    # Escape label (defined in utils.R)
    escaped_label <- .escape_typst_string(current_entry$label)

    # Escape the formatted string for inclusion in an R string literal
    # The content itself should NOT be Typst-escaped again here.
    item_for_r_string <- gsub("\\", "\\\\", current_entry$formatted_string, fixed = TRUE)
    item_for_r_string <- gsub('"', '\\"', item_for_r_string, fixed = TRUE)

    # Create the '(label: "...", item: "...")' string
    sprintf('  (label: "%s", item: "%s")', escaped_label, item_for_r_string)
  })

  # --- Assemble the final Typst block ---
  # Combine entries with commas and newlines
  typst_body <- paste(typst_entry_strings, collapse = ",\n")

  # Wrap in the Typst function call and code block
  typst_block <- sprintf("```{=typst}\n#%s((\n%s\n))\n```", typst_func_name, typst_body)

  return(typst_block)
}

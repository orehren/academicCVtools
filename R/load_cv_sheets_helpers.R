# R/load_cv_sheets_helpers.R

#' Prepare sheet names and target names for loading
#'
#' Determines the sheets to read and the names for the resulting list
#' based on the structure of the sheets_to_load argument.
#' Assumes basic validation of sheets_to_load has already occurred.
#'
#' @param sheets_to_load A named list/vector, an unnamed character vector, or the string "*".
#' @param ss_input The resolved Google Sheet identifier (ID string, URL string, or dribble),
#'        needed if sheets_to_load is "*".
#'
#' @return A list containing two elements:
#'         - `sheet_names_to_read`: Character vector of sheet names.
#'         - `target_list_names`: Character vector for naming the output list.
#'
#' @importFrom rlang is_named
#' @importFrom cli cli_inform
#' @importFrom googlesheets4 sheet_names
#'
#' @noRd
.prepare_sheet_load_config <- function(sheets_to_load, ss_input = NULL) { # ss_input hinzugefügt

  sheet_names_to_read <- NULL
  target_list_names <- NULL

  is_special_wildcard <- is.character(sheets_to_load) && length(sheets_to_load) == 1 && sheets_to_load == "*"

  if (is_special_wildcard) {
    # Fetch all sheet names from the document
    if (is.null(ss_input)) {
      # This should not happen if called correctly from load_cv_sheets
      cli::cli_abort("Internal error: {.arg ss_input} must be provided when {.arg sheets_to_load} is \"*\".", call. = FALSE)
    }
    cli::cli_inform("Fetching all sheet names from the document...")
    sheet_names_to_read <- tryCatch({
      googlesheets4::sheet_names(ss_input)
    }, error = function(e) {
      cli::cli_abort("Failed to retrieve sheet names from the document when 'sheets_to_load = \"*\"'.", parent = e, call. = FALSE)
    })

    if (length(sheet_names_to_read) == 0) {
      cli::cli_warn("No sheets found in the document or failed to retrieve names.")
      # Return empty config to produce empty list later
      return(list(sheet_names_to_read = character(0), target_list_names = character(0)))
    }

    # Generate target names from fetched sheet names
    target_list_names <- gsub("[^[:alnum:]_]+", "_", tolower(sheet_names_to_read))
    cli::cli_inform("Using all {length(sheet_names_to_read)} sheets found: {paste(shQuote(sheet_names_to_read), collapse = ', ')}. Target list names: {.val {target_list_names}}")

  } else {
    # Existing logic for named list/vector or unnamed character vector
    is_named_input <- rlang::is_named(sheets_to_load)
    if (is_named_input) {
      sheet_names_to_read <- names(sheets_to_load)
      target_list_names <- as.character(unlist(sheets_to_load))
    } else {
      sheet_names_to_read <- sheets_to_load
      target_list_names <- gsub("[^[:alnum:]_]+", "_", tolower(sheet_names_to_read))
      cli::cli_inform(
        "Using cleaned sheet names as names for the returned list: {.val {target_list_names}}"
      )
    }
  }

  return(list(
    sheet_names_to_read = sheet_names_to_read,
    target_list_names = target_list_names
  ))
}


#' Load data for multiple sheets iteratively
#' Calls read_cv_sheet for each sheet name provided.
#' Assumes arguments have been validated.
#' @param sheet_names_to_read Character vector of sheet names.
#' @param doc_identifier The resolved document identifier (ID, URL, dribble).
#' @param ... Additional arguments intended for read_cv_sheet.
#' @return A list of tibbles, one for each sheet read. Aborts on failure.
#' @noRd
#' @importFrom purrr map
#' @importFrom rlang exec list2
#' @importFrom cli cli_inform
.load_sheets_data <- function(sheet_names_to_read, doc_identifier, ...) {

  cli::cli_inform("Starting data loading for {length(sheet_names_to_read)} sheet(s)...")

  # Capture the dots arguments explicitly using rlang::list2
  # list2 handles dots more robustly, especially regarding defaults
  dot_args <- rlang::list2(...)

  loaded_data_list <- purrr::map(sheet_names_to_read, function(current_sheet_name) {

    cli::cli_inform("--> Loading sheet {.val {current_sheet_name}}...")

    # Prepare arguments for read_cv_sheet
    # Start with fixed arguments
    args_for_read <- list(
      doc_identifier = doc_identifier,
      sheet_name = current_sheet_name
    )

    # Add arguments from dots, potentially overriding defaults if present in dots
    # Combine lists, arguments in dot_args take precedence
    final_args <- c(args_for_read, dot_args)

    # Call read_cv_sheet using do.call or rlang::exec with the explicit list
    # do.call is often simpler here:
    sheet_data <- do.call(read_cv_sheet, final_args)

    # Alternative with rlang::exec:
    # sheet_data <- rlang::exec("read_cv_sheet", !!!final_args)

    return(sheet_data)
  })

  cli::cli_inform("...Data loading finished.")
  return(loaded_data_list)
}

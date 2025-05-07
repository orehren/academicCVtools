# R/load_cv_sheets_helpers.R

#' Prepare sheet names and target names for loading
#'
#' Determines the sheets to read and the names for the resulting list
#' based on the structure of the sheets_to_load argument.
#' Assumes basic validation of sheets_to_load has already occurred.
#'
#' @param sheets_to_load A named list/vector OR an unnamed character vector.
#'
#' @return A list containing two elements:
#'         - `sheet_names_to_read`: Character vector of sheet names.
#'         - `target_list_names`: Character vector for naming the output list.
#'
#' @importFrom rlang is_named check_installed
#' @importFrom janitor make_clean_names
#' @importFrom cli cli_inform
#'
#' @noRd
.prepare_sheet_load_config <- function(sheets_to_load) {

  is_named_input <- rlang::is_named(sheets_to_load)

  sheet_names_to_read <- NULL
  target_list_names <- NULL

  if (is_named_input) {
    sheet_names_to_read <- names(sheets_to_load)
    # Ensure values are used as names, force to character
    target_list_names <- as.character(unlist(sheets_to_load))
  } else {
    # Input is unnamed character vector (validation ensures this)
    sheet_names_to_read <- sheets_to_load
    # Check dependency and generate names
    rlang::check_installed("janitor", reason = "to automatically generate list names from sheet names when 'sheets_to_load' is unnamed.")
    target_list_names <- janitor::make_clean_names(sheet_names_to_read)
    cli::cli_inform(
      "Using cleaned sheet names as names for the returned list: {.val {target_list_names}}"
    )
  }

  # Return both vectors in a list
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

# R/utils.R

#' Escape special characters in a string for safe inclusion in a Typst string.
#'
#' This function escapes backslashes and double quotes.
#'
#' @param text The string to escape.
#' @return The escaped string.
#' @noRd
.escape_typst_string <- function(text) {
  text <- gsub("\\", "\\\\", text, fixed = TRUE)
  text <- gsub("\"", "\\\"", text, fixed = TRUE)
  text
}

#' Find and validate an executable's path
#'
#' Checks for an executable in the system's PATH or at a user-specified path.
#' Throws a clear error if the executable cannot be found or is not executable.
#'
#' @param exec_name The name of the executable (e.g., "pandoc").
#' @param user_path An optional, user-provided path to the executable.
#' @param arg_name The name of the user-facing argument for the path (for errors).
#'
#' @return The validated, absolute path to the executable.
#' @importFrom cli cli_abort
#' @noRd
.validate_executable_found <- function(exec_name, user_path = NULL, arg_name = "path") {
  # If a user path is provided, check it first.
  if (!is.null(user_path)) {
    if (file.exists(user_path) && !dir.exists(user_path)) {
      # TODO: Check for execute permissions on non-Windows systems.
      return(normalizePath(user_path))
    } else {
      cli::cli_abort(
        "Executable not found at the path provided in {.arg {arg_name}}: {.path {user_path}}."
      )
    }
  }

  # If no user path, search the system PATH.
  system_path <- Sys.which(exec_name)
  if (system_path != "") {
    return(system_path)
  }

  # If not found anywhere, abort.
  cli::cli_abort(
    c("Could not find the {.val {exec_name}} executable.",
      "i" = "Please install {.val {exec_name}} or provide its path via the {.arg {arg_name}} argument."
    )
  )
}

#' Evaluate a tidyselect expression within a data context safely
#'
#' This helper function captures a tidyselect expression (as a quosure)
#' and evaluates it against the column names of the provided data frame.
#' It returns a character vector of the selected column names.
#' If the quosure is empty/NULL or the selection results in an empty list,
#' it returns an empty character vector.
#' It provides a clear error message if tidyselect fails (e.g., column not found).
#'
#' @param tidy_quo A quosure containing the tidyselect expression
#'        (e.g., `quo(c(col1, starts_with("detail")))`).
#' @param data The data frame whose column names are the context for evaluation.
#' @param arg_name The name of the original user-facing argument (for error messages).
#'
#' @return A character vector of selected column names.
#' @importFrom rlang quo_is_null eval_tidy
#' @importFrom tidyselect eval_select
#' @importFrom cli cli_abort
#' @noRd
evaluate_tidyselect_safely <- function(tidy_quo, data, arg_name) {
  # If the user provided NULL or an empty expression, return an empty vector
  if (rlang::quo_is_null(tidy_quo)) {
    return(character(0))
  }

  tryCatch(
    {
      # Evaluate the tidyselect expression
      selected_indices <- tidyselect::eval_select(tidy_quo, data)
      # Get names from indices
      selected_names <- names(selected_indices)

      # Handle cases where selection is valid but returns no columns (e.g., starts_with("nonexistent"))
      if (length(selected_names) == 0) {
        return(character(0))
      }

      return(selected_names)
    },
    # Catch specific tidyselect errors (like 'object not found')
    error = function(e) {
      cli::cli_abort(
        "Failed to select columns for the {.arg {arg_name}} argument: {e$message}",
        parent = e,
        call = NULL # Hide the internal call stack from the user
      )
    }
  )
}

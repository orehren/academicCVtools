# R/utils.R - Internal Helper Functions

#' Check if an argument is a single, non-empty string
#' @param arg_value The value of the argument to check.
#' @param arg_name The name of the argument (as a string) for error messages.
#' @importFrom cli cli_abort
#' @importFrom rlang caller_arg caller_env is_list %||%
#' @importFrom stringr str_replace_all
#' @importFrom magrittr %>%
#' @importFrom purrr map_chr
#'
#'
#' @noRd
check_single_string <- function(arg_value, arg_name) {
  arg_expr <- rlang::caller_arg(arg_value)
  if (!is.character(arg_value) || length(arg_value) != 1 || is.na(arg_value) || nchar(arg_value) == 0) {
    cli::cli_abort(
      "Argument {.arg {arg_name}} must be a single, non-empty character string. Problem with call: {.code {arg_expr}}",
      call = rlang::caller_env(n = 2)
    )
  }
}

#' Validate executable exists
#' @param exec_name Name of the executable (e.g., "pandoc").
#' @param path_override Optional specific path to check first.
#' @param arg_name Name of the related argument in the calling function.
#' @return The validated, absolute path to the executable.
#' @noRd
.validate_executable_found <- function(exec_name, path_override = NULL, arg_name = "executable") {
  exec_path <- if (!is.null(path_override)) path_override else Sys.which(exec_name)
  if (is.null(exec_path) || exec_path == "" || !file.exists(exec_path)) {
    msg <- sprintf("%s executable not found.", exec_name)
    if (!is.null(path_override)) {
      msg <- sprintf("%s not found at specified path: %s", exec_name, path_override)
    } else {
      msg <- sprintf("%s not found in system PATH. Please ensure %s is installed and accessible.", exec_name, exec_name)
    }
    cli::cli_abort(msg, call = rlang::caller_env(n = 2))
  }
  return(unname(exec_path))
}

# ==============================================================================
# String Helpers
# ==============================================================================

#' Escape strings for Typst string literals
#' Escapes backslashes and double quotes.
#' @param s The input character string.
#' @return The escaped string, safe for use within Typst double quotes. Returns "" if input is NA.
#' @noRd
.escape_typst_string <- function(s) {
  if (is.na(s)) {
    return("")
  }
  s %>%
    stringr::str_replace_all("\\\\", "\\\\\\\\") %>%
    stringr::str_replace_all("\"", "\\\\\"")
}


# ==============================================================================
# Extraction helpers
# ==============================================================================

#' Recursively render Pandoc AST nodes to a plain text string
#' Handles common inline and block elements.
#' @param node A list representing a single AST node.
#' @return A character string representation of the node's content.
#' @noRd
.render_ast_node_to_string <- function(node) {
  # Initial check if node is a list
  if (!rlang::is_list(node)) {
    return("")
  }

  # --- Get node type safely ---
  node_type <- node$t

  # --- Check if node_type is valid before proceeding ---
  if (is.null(node_type) || length(node_type) != 1 || !is.character(node_type)) {
    # If no valid type, try processing content if it's a simple list of nodes
    # This handles cases where a list might contain renderable items directly
    if (rlang::is_list(node$c)) {
      # Try rendering content anyway, might be a list of inlines/blocks
      return(paste0(purrr::map_chr(node$c, .render_ast_node_to_string), collapse = ""))
    } else {
      # Otherwise, cannot process this node
      return("")
    }
  }
  # --- End type check ---

  # Get content safely
  node_content <- node$c %||% NULL

  # --- Process based on valid node_type ---
  if (node_type == "Str") {
    return(node_content %||% "")
  }
  if (node_type == "Space") {
    return(" ")
  }
  if (node_type == "SoftBreak") {
    return(" ")
  }
  if (node_type == "LineBreak") {
    return("\n")
  }

  # Handle container elements (where node_content is a list of nodes)
  if (rlang::is_list(node_content)) {
    if (node_type %in% c(
      "Emph", "Strong", "Superscript", "Subscript",
      "SmallCaps", "Quoted", "Para", "Plain"
    )) {
      return(paste0(purrr::map_chr(node_content, .render_ast_node_to_string), collapse = ""))
    }
    # Handle Span and Link (content is nested differently)
    if (node_type == "Span" && length(node_content) >= 2 && rlang::is_list(node_content[[2]])) {
      return(paste0(purrr::map_chr(node_content[[2]], .render_ast_node_to_string), collapse = ""))
    }
    if (node_type == "Link" && length(node_content) >= 1 && rlang::is_list(node_content[[1]])) {
      return(paste0(purrr::map_chr(node_content[[1]], .render_ast_node_to_string), collapse = "")) # Link text only
    }
  }

  # Handle elements where content is scalar (usually in [[2]])
  if (node_type %in% c("Code", "RawInline", "Math") && length(node_content) >= 2) {
    # Ensure content is character before returning
    scalar_content <- node_content[[2]]
    return(as.character(scalar_content %||% ""))
  }

  # Default for unhandled types
  # Optional: Warn about unhandled types?
  # cli::cli_warn("Unhandled AST node type: {node_type}")
  return("")
}


#' Safely get a potentially nested value from a list structure
#' Handles cases where path elements might be missing or NULL.
#' Designed for structures like Pandoc JSON AST where values are often in list$c.
#' @param data_list The list to access.
#' @param path A character vector specifying the nested names to access
#'        (e.g., c("references", "c", 1, "c", "id", "c")).
#'        Numeric elements indicate list indices.
#' @return The extracted value, or `default` if the path is invalid or value is NULL.
#' @noRd
#' @importFrom purrr reduce possibly
#' @importFrom rlang is_list
.safe_get_value <- function(data_list, path, default = NA) {
  # Define a safe accessor function for reduce
  # It returns NULL if a level is missing or not indexable
  safe_accessor <- function(current_level, index_or_name) {
    if (is.null(current_level)) {
      return(NULL)
    }

    # Use `[[` which works for names and indices and returns NULL if missing
    # Wrap in possibly to handle errors during access (e.g., wrong type)
    possibly_accessor <- purrr::possibly(~ .x[[index_or_name]], otherwise = NULL)
    return(possibly_accessor(current_level))
  }

  # Use reduce to traverse the path
  final_value <- purrr::reduce(path, safe_accessor, .init = data_list)

  # Return default if NULL or the value itself
  return(final_value %||% default)
}


#' Evaluate a tidyselect expression safely within a data context
#' @param quo A quosure containing the tidyselect expression (captured with enquo).
#' @param data The data frame/tibble providing the context for column names.
#' @param arg_name_str The name of the original argument (as a string) for error messages.
#' @return A character vector of the selected column names. Returns empty vector `character(0)`
#'         if the expression is NULL or selects nothing. Throws error on evaluation failure.
#' @noRd
#' @importFrom tidyselect eval_select
#' @importFrom cli cli_abort
#' @importFrom rlang quo_is_null eval_tidy caller_env
evaluate_tidyselect_safely <- function(quo, data, arg_name_str) {
  # If the quosure is NULL (meaning the argument was likely NULL), return empty vector
  if (rlang::quo_is_null(quo)) {
    return(character(0))
  }

  col_indices <- tryCatch(
    {
      # Use eval_tidy here as eval_select needs the quosure evaluated
      tidyselect::eval_select(quo, data)
    },
    error = function(e) {
      cli::cli_abort(
        "Failed to evaluate argument {.arg {arg_name_str}} using tidyselect.",
        parent = e, # Keep the original error context
        call = rlang::caller_env(n = 3) # Go up 3 levels: helper -> format_typst_section -> user call
      )
    }
  )

  # Return names corresponding to the selected indices
  # Handle case where nothing is selected (returns integer(0))
  if (length(col_indices) == 0) {
    return(character(0))
  } else {
    return(names(data)[col_indices])
  }
}


#' Check multiple arguments are single, non-empty strings
#' Iterates over a named list where names are argument names (as strings)
#' and values are the argument values to be checked.
#' Calls check_single_string for each item.
#' @param args_list A named list (e.g., list(arg1 = value1, arg2 = value2)).
#' @noRd
#' @importFrom purrr iwalk
check_multiple_single_strings <- function(args_list) {
  # Use iwalk to iterate over names (.y) and values (.x) of the list
  purrr::iwalk(args_list, ~ check_single_string(arg_value = .x, arg_name = .y))
  # No explicit return needed, works via side-effect (potential abort)
}


#' Combine start and end date columns into a formatted date range string
#'
#' This function takes a data frame and the names of two columns containing
#' start and end dates (expected to be R Date objects or coercible to Date).
#' It creates a new column with a formatted string representing the date range
#' (e.g., "YYYY - YYYY", "MM/YYYY - Present").
#'
#' @param data A data frame or tibble.
#' @param start_col The name of the column containing the start dates (character).
#' @param end_col The name of the column containing the end dates (character).
#' @param output_col The name for the new column that will contain the
#'        formatted date range string (character). Defaults to `"date_range"`.
#' @param output_format A string specifying the desired output format for the
#'        dates, using standard R `strftime` specifiers (e.g., `"%Y"`, `"%m/%Y"`,
#'        `"%d/%m/%Y"`). Defaults to `"%m/%Y"`.
#' @param sep The separator string to use between the formatted start and
#'        end dates. Defaults to `" - "`.
#' @param ongoing_label The string to use for the end date if it is `NA` or
#'        interpreted as ongoing. Defaults to `"Present"`.
#'
#' @return The input data frame with an added column containing the
#'         formatted date range strings.
#'
#' @importFrom dplyr mutate rowwise %>% pull case_when
#' @importFrom rlang sym !! :=
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- dplyr::tibble(
#'   start_date = as.Date(c("2020-01-01", "2021-05-01", NA, "2022-03-01")),
#'   completion_date = as.Date(c("2020-12-31", NA, "2021-08-15", "2022-03-01"))
#' )
#'
#' # Basic usage
#' format_date_range(df, "start_date", "completion_date")
#'
#' # Different output format and column name
#' format_date_range(df, "start_date", "completion_date",
#'                   output_col = "Period", output_format = "%Y")
#'
#' # Different separator and ongoing label
#' format_date_range(df, "start_date", "completion_date",
#'                   sep = " to ", ongoing_label = "Current")
#'
#' # Handle cases where start_date might be a string coercible to Date
#' df_string <- dplyr::tibble(
#'   begin = c("2020-01-01", "2021/05/01"),
#'   end = c("2020-12-31", NA)
#' )
#' format_date_range(df_string, "begin", "end")
#' }
format_date_range <- function(data,
                              start_col,
                              end_col,
                              output_col = "date_range",
                              output_format = "%m/%Y",
                              sep = " - ",
                              ongoing_label = "Present") {

  .validate_format_date_range_args(
    data, start_col, end_col, output_col,
    output_format, sep, ongoing_label
  )

  start_col_sym <- rlang::sym(start_col)
  end_col_sym <- rlang::sym(end_col)
  output_col_sym <- rlang::sym(output_col)

  expected_date_formats <- c(
    "Ymd", "Y-m-d", "Y/m/d",
    "dmY", "d-m-Y", "d/m/Y",
    "mdY", "m-d-Y", "m/d/Y",
    "Ym", "Y-m", "Y/m",
    "mY", "m-Y", "m/Y",
    "Y"
  )

  data_processed <- data %>%
    dplyr::mutate(
      !!output_col_sym := dplyr::rowwise(.) %>%
        dplyr::mutate(
          start_dt_val = !!start_col_sym,
          end_dt_val = !!end_col_sym,

          start_dt = .parse_to_posixct_or_na(start_dt_val, expected_date_formats),
          end_dt = .parse_to_posixct_or_na(end_dt_val, expected_date_formats),

          start_str = dplyr::case_when(
            !is.na(start_dt) ~ format(as.Date(start_dt), output_format),
            TRUE ~ ""
          ),
          end_str = dplyr::case_when(
            !is.na(end_dt) ~ format(as.Date(end_dt), output_format),
            is.na(end_dt) && !is.na(start_dt) ~ ongoing_label,
            TRUE ~ ""
          ),
          date_range_temp = dplyr::case_when(
            nzchar(start_str) && nzchar(end_str) && end_str != ongoing_label ~ paste0(start_str, sep, end_str),
            nzchar(start_str) && end_str == ongoing_label ~ paste0(start_str, sep, end_str),
            nzchar(start_str) && !nzchar(end_str) ~ start_str,
            !nzchar(start_str) && nzchar(end_str) && end_str != ongoing_label ~ end_str,
            TRUE ~ ""
          )
        ) %>%
        dplyr::pull(date_range_temp)
    )

  return(data_processed)
}


#' Date/Time Helper: Parse a value to POSIXct or return POSIXct NA
#'
#' Attempts to parse a value (which can be a Date, POSIXt, or string)
#' into a POSIXct object using a list of expected formats.
#' Returns NA (of type POSIXct) if parsing fails or input is NA/empty.
#'
#' @param value The value to parse.
#' @param formats A character vector of date-time formats to try with
#'        `lubridate::parse_date_time`.
#' @return A POSIXct object or `as.POSIXct(NA)`.
#'
#' @importFrom lubridate is.Date is.POSIXt parse_date_time
#'
#' @noRd
.parse_to_posixct_or_na <- function(value, formats) {
  # If already a Date object, convert to POSIXct (at midnight UTC)
  if (lubridate::is.Date(value)) {
    return(as.POSIXct(value, tz = "UTC"))
  }
  # If already a POSIXt object, return as is
  if (lubridate::is.POSIXt(value)) {
    return(value)
  }
  # Handle NA, NULL, or empty strings before attempting to parse
  if (is.na(value) || is.null(value) || !nzchar(as.character(value))) {
    return(as.POSIXct(NA_real_)) # Explicit POSIXct NA
  }

  # Attempt to parse the string using the provided formats
  parsed_date <- suppressWarnings(
    lubridate::parse_date_time(as.character(value),
                               orders = formats,
                               quiet = TRUE,
                               tz = "UTC") # Specify timezone for consistency
  )

  # Ensure NA result is also POSIXct NA
  if (is.na(parsed_date)) {
    return(as.POSIXct(NA_real_))
  }

  return(parsed_date)
}

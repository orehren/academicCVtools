# R/format_typst_section.R

#' Format CV section data into a Typst string
#'
#' Takes a data frame (tibble) representing a CV section and formats it into
#' a single string suitable for Typst. Each row of the data frame is converted
#' into a Typst function call. Columns can be combined or excluded using
#' tidyselect syntax. Argument validation is performed using assertions from
#' the `checkmate` package via an internal helper function.
#'
#' @param data A data frame or tibble where each row is a CV entry and columns
#'        are fields. Typically the output of `read_cv_sheet` or an element
#'        from the list returned by `load_cv_sheets`. Must not be `NULL`.
#' @param typst_func The name of the Typst function to call for each entry
#'        (row). Must be a single string starting with `#`, e.g., `"#cvEntry"`.
#' @param combine_cols Optional: A vector of column names or a tidyselect
#'        expression (e.g., `c(detail_1, detail_2)` or `starts_with("detail")`)
#'        specifying columns to combine into a single field. Defaults to `NULL`
#'        (no combination).
#' @param combine_as Character string: The name of the new field in the Typst
#'        dictionary that will contain the combined values. Defaults to `"details"`.
#'        Must be a single, non-empty string.
#' @param combine_sep Character string: The separator used when joining the
#'        values from `combine_cols`. Defaults to `"\\n"` (newline). Can be empty.
#' @param combine_prefix Character string: A prefix added to each non-NA value
#'        before joining. Defaults to `"- "`. Can be empty.
#' @param exclude_cols Optional: A vector of column names or a tidyselect
#'        expression specifying columns to exclude from the Typst output.
#'        Defaults to `NULL` (no exclusion). Columns listed in `combine_cols`
#'        are automatically excluded from the main output dictionary.
#' @param output_mode Character string:
#' @param na_action Character string: How to handle `NA` values in columns *not*
#'        involved in `combine_cols`. Must be one of `"omit"`, `"keep"`, or `"string"`.
#'        - `"omit"` (default): Key-value pairs with `NA` values are omitted.
#'        - `"keep"`: `NA` values are converted to Typst's `none` literal.
#'        - `"string"`: `NA` values are converted to the string `"NA"`.
#' @param output_mode Character string: Defines the output structure.
#'        Must be one of `"rowwise"` (default) or `"array"`.
#'        - `"rowwise"`: Generates a separate Typst function call for each row.
#'        - `"array"`: Generates a single Typst function call passing an array
#'          of all row dictionaries.
#' @return A single character string containing Typst code. This string consists
#'         of multiple Typst function calls (one per row of `data`), wrapped
#'         in a ```{typst} ... ``` block. Returns an empty Typst block
#'         `"```{typst}\n```"` if the input data frame has zero rows after
#'         validation. Throws an error if arguments are invalid.
#'
#' @importFrom dplyr select mutate across all_of relocate row_number %>%
#' @importFrom tidyr unite pivot_longer
#' @importFrom purrr pmap_chr map_chr keep discard map
#' @importFrom stringr str_c str_replace_all str_glue
#' @importFrom rlang enquo is_empty syms !!! parse_exprs quos expr quo_is_null arg_match
#' @importFrom tidyselect everything
#' @importFrom cli cli_warn cli_inform cli_abort
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # --- Setup: Create example data ---
#' example_data <- dplyr::tibble(
#'   Date = c("2023-01-01", "2022-05-15", "2021-11-30"),
#'   Title = c("Project Alpha", "Research Grant", "Initial Concept"),
#'   Location = c("Lab A", "University X", "Office B"),
#'   Description = c("Developed core module.", "Secured funding.", "Drafted proposal."),
#'   Detail_A = c("Used R and Python", "Wrote application", "Market research"),
#'   Detail_B = c("Met all deadlines", NA, "Competitor analysis"),
#'   Status = c("Completed", "Ongoing", "Completed"),
#'   Notes = c("Successful deployment", "Reporting due Q4", NA)
#' )
#'
#' # Load the package if not already loaded
#' # library(academicCVtools)
#' # Or use devtools::load_all() during development
#'
#' # --- Example 1: Basic usage ---
#' # All columns used, NA omitted (default)
#' cat(format_typst_section(example_data, typst_func = "#basicEntry"))
#'
#' # --- Example 2: Combining detail columns ---
#' # Combine Detail_A and Detail_B into "key_points"
#' cat(format_typst_section(
#'   data = example_data,
#'   typst_func = "#projectEntry",
#'   combine_cols = c(Detail_A, Detail_B), # Tidyselect: Explicit names
#'   combine_as = "key_points",
#'   combine_prefix = "* "
#' ))
#'
#' # --- Example 3: Combining with starts_with ---
#' # Combine all columns starting with "Detail_"
#' cat(format_typst_section(
#'   data = example_data,
#'   typst_func = "#projectEntry",
#'   combine_cols = starts_with("Detail_"), # Tidyselect: Helper function
#'   combine_as = "details_combined",
#'   combine_prefix = "- "
#' ))
#'
#' # --- Example 4: Excluding columns ---
#' # Exclude 'Status' and 'Notes'
#' cat(format_typst_section(
#'   data = example_data,
#'   typst_func = "#simpleEntry",
#'   exclude_cols = c(Status, Notes) # Tidyselect: Explicit names
#' ))
#'
#' # --- Example 5: Combining AND Excluding ---
#' # Combine details, exclude Location, Status, and Notes
#' cat(format_typst_section(
#'   data = example_data,
#'   typst_func = "#focusedEntry",
#'   combine_cols = starts_with("Detail_"),
#'   combine_as = "key_details",
#'   exclude_cols = c(Location, Status, Notes)
#' ))
#'
#' # --- Example 6: Handling NA as 'none' ---
#' cat(format_typst_section(
#'   data = example_data,
#'   typst_func = "#entryWithNone",
#'   na_action = "keep"
#' ))
#'
#' # --- Example 7: Handling NA as string "NA" ---
#' cat(format_typst_section(
#'   data = example_data,
#'   typst_func = "#entryWithStringNA",
#'   na_action = "string"
#' ))
#'
#' # --- Example 8: Empty Input Data ---
#' # Should return empty Typst block without warning (checked in validator)
#' cat(format_typst_section(
#'   data = example_data[0, ], # Empty data frame
#'   typst_func = "#emptyEntry"
#' ))
#'
#' # --- Example 9: Invalid Tidyselect (should error via helper) ---
#' tryCatch(
#'   format_typst_section(
#'     data = example_data,
#'     typst_func = "#errorEntry",
#'     exclude_cols = c(Status, NonExistentCol)
#'   ),
#'   error = function(e) print(paste("Successfully caught expected error:", e$message))
#' )
#'
#' # --- Example 10: Invalid Argument (should error via checkmate) ---
#' tryCatch(
#'   format_typst_section(
#'     data = example_data,
#'     typst_func = "NotStartingWithHash" # Invalid typst_func
#'   ),
#'   error = function(e) print(paste("Successfully caught expected error:", e$message))
#' )
#' }
format_typst_section <- function(data,
                                 typst_func,
                                 combine_cols = NULL,
                                 combine_as = "details",
                                 combine_sep = "\\n",
                                 combine_prefix = "- ",
                                 exclude_cols = NULL,
                                 na_action = c("omit", "keep", "string"),
                                 output_mode = c("rowwise", "array")) {

  # ============================================================================
  # Phase 0: Validate Arguments
  # ============================================================================
  combine_cols_quo <- rlang::enquo(combine_cols)
  exclude_cols_quo <- rlang::enquo(exclude_cols)

  temp_output_mode <- output_mode # Store original for validator
  if(length(output_mode) > 1) output_mode <- output_mode[1]

  na_action <- match.arg(na_action)

  output_mode <- match.arg(output_mode)

  # Call the dedicated validation function (defined in validation_helpers.R)
  .validate_format_typst_section_args(
    data, typst_func, combine_cols_quo, combine_as, combine_sep,
    combine_prefix, exclude_cols_quo, na_action, output_mode
  )

  # ============================================================================
  # Phase 1: Handle Empty Data Input
  # ============================================================================
  if (nrow(data) == 0) {
    return("```{=typst}\n```")
  }

  # ============================================================================
  # Phase 2: Evaluate Tidyselect Expressions
  # ============================================================================
  # Use helper from utils.R (evaluate_tidyselect_safely)
  combine_col_names <- evaluate_tidyselect_safely(combine_cols_quo, data, "combine_cols")
  exclude_col_names <- evaluate_tidyselect_safely(exclude_cols_quo, data, "exclude_cols")

  # ============================================================================
  # Phase 3: Data Preparation (using Helpers)
  # ============================================================================
  # Start with the original validated data
  data_proc <- data

  # 3a. Combine columns using helper
  data_proc <- .combine_columns_data(
    data_proc = data_proc,
    combine_col_names = combine_col_names,
    combine_as = combine_as,
    combine_sep = combine_sep,
    combine_prefix = combine_prefix
  )

  # 3b. Exclude columns using helper
  # Ensure combine_as is not accidentally excluded if it was listed in exclude_cols
  if (combine_as %in% exclude_col_names) {
    exclude_col_names <- setdiff(exclude_col_names, combine_as)
    # Inform user (optional, could be moved to validator or helper)
    # cli::cli_inform("Column {.val {combine_as}} was listed in {.arg exclude_cols} but is kept as it's the target of {.arg combine_cols}.")
  }
  data_proc <- .exclude_columns_data(
    data_proc = data_proc,
    exclude_col_names = exclude_col_names
  )

  # ============================================================================
  # Phase 4: Typst String Generation (using Helper)
  # ============================================================================
  typst_lines <- .generate_typst_rows(
    data_proc = data_proc,
    typst_func = typst_func,
    na_action = na_action,
    output_mode = output_mode
  )

  # ============================================================================
  # Phase 5: Final Output Assembly
  # ============================================================================

  # Handle case where generate_typst_rows returns empty vector (all rows filtered)
  if (length(typst_lines) == 0) {

    if (output_mode == "array") {
      return(stringr::str_glue("```{{=typst}}\n{typst_func}(())\n```"))
    } else {
      return("```{=typst}\n```")
    }
  }

  # Assemble the final Typst block
  if (output_mode == "rowwise") {
    final_typst_string <- stringr::str_glue(
      "```{{=typst}}\n",
      "{stringr::str_c(typst_lines, collapse = '\\n')}\n",
      "```"
    )
  } else {
    array_content <- stringr::str_c(typst_lines, collapse = ",\n  ")
    final_typst_string <- stringr::str_glue(
      "```{{=typst}}\n",
      "{typst_func}((\n  {array_content}\n))\n",
      "```"
    )
  }


  return(final_typst_string)
}

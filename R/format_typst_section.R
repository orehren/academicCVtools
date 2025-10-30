# R/format_typst_section.R

#' Format CV section data into a Typst string
#'
#' Takes a data frame (tibble) representing a CV section and formats it into
#' a single string suitable for Typst. Each row of the data frame is converted
#' into a Typst function call. Columns can be combined or excluded using
#' tidyselect syntax.
#'
#' @param data A data frame or tibble where each row is a CV entry.
#' @param typst_func The name of the Typst function to call for each entry.
#' @param combine_cols Tidyselect expression for columns to combine.
#' @param combine_as Name of the new combined column.
#' @param combine_sep Separator for combined columns.
#' @param combine_prefix Prefix for combined column values.
#' @param exclude_cols Tidyselect expression for columns to exclude.
#' @param na_action How to handle `NA` values: "omit", "keep", or "string".
#' @param output_mode Output structure: "rowwise" or "array".
#'
#' @return A single character string containing Typst code.
#'
#' @importFrom dplyr select mutate across all_of relocate row_number
#' @importFrom tidyr unite pivot_longer
#' @importFrom rlang enquo arg_match sym
#' @importFrom tidyselect everything
#' @export
format_typst_section <- function(data,
                                 typst_func,
                                 combine_cols = NULL,
                                 combine_as = "details",
                                 combine_sep = "\\ ",
                                 combine_prefix = "- ",
                                 exclude_cols = NULL,
                                 na_action = c("omit", "keep", "string"),
                                 output_mode = c("rowwise", "array")) {
  # --- Phase 0: Argument Matching and Validation ---
  na_action <- rlang::arg_match(na_action)
  output_mode <- rlang::arg_match(output_mode)

  combine_cols_quo <- rlang::enquo(combine_cols)
  exclude_cols_quo <- rlang::enquo(exclude_cols)

  .validate_format_typst_section_args(
    data, typst_func, combine_cols_quo, combine_as, combine_sep,
    combine_prefix, exclude_cols_quo, na_action, output_mode
  )

  # --- Phase 1: Handle Empty Data ---
  if (nrow(data) == 0) {
    return("```{=typst}\n```")
  }

  # --- Phase 2: Data Preparation ---
  combine_col_names <- evaluate_tidyselect_safely(combine_cols_quo, data, "combine_cols")
  exclude_col_names <- evaluate_tidyselect_safely(exclude_cols_quo, data, "exclude_cols")

  data_proc <- data

  if (length(combine_col_names) > 0) {
    data_proc <- data_proc |>
      dplyr::mutate(dplyr::across(dplyr::all_of(combine_col_names), ~ifelse(is.na(.), NA, paste0(combine_prefix, .)))) |>
      tidyr::unite(
        col = !!rlang::sym(combine_as),
        dplyr::all_of(combine_col_names),
        sep = combine_sep,
        na.rm = TRUE,
        remove = TRUE
      )
  }

  if (length(exclude_col_names) > 0) {
    data_proc <- dplyr::select(data_proc, -dplyr::all_of(exclude_col_names))
  }

  # --- Phase 3: Vectorized String Generation ---
  typst_dictionaries <- data_proc |>
    dplyr::mutate(.row_id = dplyr::row_number()) |>
    tidyr::pivot_longer(
      cols = -".row_id",
      names_to = "key",
      values_to = "value",
      values_transform = as.character
    ) |>
    dplyr::mutate(
      value_type = ifelse(is.na(.data$value), na_action, "string"),
      value = dplyr::case_when(
        .data$value_type == "omit" ~ NA_character_,
        .data$value_type == "keep" ~ "none",
        .data$value_type == "string" & is.na(.data$value) ~ '"NA"',
        .data$value_type == "string" ~ paste0('"', .escape_typst_string(.data$value), '"')
      )
    ) |>
    dplyr::filter(!is.na(.data$value)) |>
    dplyr::group_by(.data$.row_id) |>
    dplyr::summarise(
      dict_str = paste0(
        "(", paste(.data$key, .data$value, sep = ": ", collapse = ", "), ")"
      )
    ) |>
    dplyr::pull(.data$dict_str)

  # --- Phase 4: Final Output Assembly ---
  if (length(typst_dictionaries) == 0) {
    return(if (output_mode == "array") sprintf("```{=typst}\n%s(())\n```", typst_func) else "```{=typst}\n```")
  }

  if (output_mode == "rowwise") {
    content <- paste0(typst_func, typst_dictionaries, collapse = "\n")
    return(sprintf("```{=typst}\n%s\n```", content))
  }

  # Else, array mode
  content <- paste0("  ", typst_dictionaries, collapse = ",\n")
  return(sprintf("```{=typst}\n%s((\n%s\n))\n```", typst_func, content))
}

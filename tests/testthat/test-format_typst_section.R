# tests/testthat/test-format_typst_section.R
library(testthat)
library(dplyr)
library(stringr)

# --- Setup: Create reusable example data ---
example_data <- tibble(
  Date = c("2023-01-01", "2022-05-15", "2021-11-30"),
  Title = c("Project Alpha", "Research Grant", "Initial Concept"),
  Description = c("Developed core module.", "Secured funding.", "Drafted proposal."),
  Detail_A = c("Used R and Python", "Wrote application", "Market research"),
  Detail_B = c("Met all deadlines", NA, "Competitor analysis"),
  Status = c("Completed", "Ongoing", "Completed"),
  Notes = c("Successful deployment", "Reporting due Q4", NA)
)

# --- Test Block 1: Basic case ---
test_that("format_typst_section works for basic case with default NA handling", {
  actual_output <- format_typst_section(example_data, typst_func = "#cvEntry")

  # Snapshot for overall structure
  expect_snapshot(cat(actual_output))

  # Specific checks
  expect_true(str_detect(actual_output, fixed('Title: "Project Alpha"')))
  expect_false(str_detect(actual_output, fixed("Detail_B: none"))) # Should be omitted
})

# --- Test Block 2: Column combination ---
test_that("format_typst_section correctly combines columns", {
  actual_output <- format_typst_section(
    data = example_data,
    typst_func = "#projectItem",
    combine_cols = c(Detail_A, Detail_B),
    combine_as = "key_points",
    combine_sep = " | "
  )

  expect_snapshot(cat(actual_output))
  expect_false(str_detect(actual_output, fixed("Detail_A:")))
  expect_true(str_detect(actual_output, fixed('key_points: "- Used R and Python | - Met all deadlines"')))
})

# --- Test Block 3: Column exclusion ---
test_that("format_typst_section correctly excludes columns", {
  actual_output <- format_typst_section(
    data = example_data,
    typst_func = "#simpleEntry",
    exclude_cols = c(Status, Notes)
  )

  expect_snapshot(cat(actual_output))
  expect_false(str_detect(actual_output, fixed("Status:")))
  expect_true(str_detect(actual_output, fixed("Title:")))
})

# --- Test Block 4: Combine and Exclude ---
test_that("format_typst_section handles combining and excluding simultaneously", {
  actual_output <- format_typst_section(
    data = example_data,
    typst_func = "#focusedItem",
    combine_cols = starts_with("Detail_"),
    combine_as = "key_info",
    exclude_cols = c(Date, Title)
  )

  expect_snapshot(cat(actual_output))
  expect_true(str_detect(actual_output, fixed("key_info:")))
  expect_false(str_detect(actual_output, fixed("Date:")))
})

# --- Test Block 5: NA handling ---
test_that("format_typst_section handles na_action correctly", {
  # na_action = "keep"
  output_na_keep <- format_typst_section(example_data, typst_func = "#itemWithNone", na_action = "keep")
  expect_snapshot(cat(output_na_keep))
  expect_true(str_detect(output_na_keep, fixed("Detail_B: none")))
  expect_true(str_detect(output_na_keep, fixed("Notes: none")))

  # na_action = "string"
  output_na_string <- format_typst_section(example_data, typst_func = "#itemWithStringNA", na_action = "string")
  expect_snapshot(cat(output_na_string))
  expect_true(str_detect(output_na_string, fixed('Detail_B: "NA"')))
})

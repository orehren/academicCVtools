# tests/testthat/test-read_cv_sheet.R
library(testthat)
library(dplyr)
library(mockery)

# --- Test Block 1: Successful Read (Name) ---
test_that("read_cv_sheet successfully reads data using a unique name", {
  expected_data <- tibble(ColA = "A")
  dummy_dribble <- structure(data.frame(name = "x", id = "y"), class = c("dribble", "data.frame"))

  stub(read_cv_sheet, ".resolve_doc_identifier", dummy_dribble)
  stub(read_cv_sheet, ".read_sheet_data", expected_data)

  actual_data <- read_cv_sheet("My Test Sheet", "MyDataSheet")
  expect_equal(actual_data, expected_data)
})

# --- Test Block 2: Successful Read (ID) ---
test_that("read_cv_sheet successfully reads data using a document ID", {
  expected_data <- tibble(X = "ID_Data")
  doc_id <- "a_valid_looking_id"

  stub(read_cv_sheet, ".resolve_doc_identifier", doc_id)
  stub(read_cv_sheet, ".read_sheet_data", expected_data)

  actual_data <- read_cv_sheet(doc_id, "DataSheetByID")
  expect_equal(actual_data, expected_data)
})

# --- Test Block 3: Error - Sheet Not Found ---
test_that("read_cv_sheet throws error when sheet_name does not exist", {
  doc_id <- "dummy_id"
  sheet_name <- "ThisSheetDoesNotExist"

  # Mock the helpers that come before the failing one
  stub(read_cv_sheet, ".resolve_doc_identifier", doc_id)

  # Mock the function that is now expected to fail
  mock_read_sheet <- mock(stop("API error: Sheet not found."))
  stub(read_cv_sheet, ".read_sheet_data", mock_read_sheet)

  expect_error(
    read_cv_sheet(doc_id, sheet_name),
    regexp = "API error: Sheet not found"
  )
  expect_called(mock_read_sheet, 1)
})

# --- Test Block 4: Warning - Empty Sheet ---
test_that("read_cv_sheet warns for empty sheet", {
  doc_id <- "doc_with_empty_sheet"
  empty_sheet_name <- "ThisSheetIsEmpty"
  empty_tibble <- tibble(Header1 = character(0))

  stub(read_cv_sheet, ".resolve_doc_identifier", doc_id)
  stub(read_cv_sheet, ".read_sheet_data", empty_tibble)

  expect_warning(
    actual_data <- read_cv_sheet(doc_id, empty_sheet_name),
    regexp = "has no data"
  )
  expect_equal(nrow(actual_data), 0)
})

# --- Test Block 5: Validation Errors ---
test_that("read_cv_sheet throws validation errors for bad inputs", {
  expect_error(
    read_cv_sheet("any_id", 123),
    regexp = "Assertion on 'sheet_name' failed"
  )
  expect_error(
    read_cv_sheet("any_id", "any_sheet", trim_ws = "TRUE"),
    regexp = "Assertion on 'trim_ws' failed"
  )
})

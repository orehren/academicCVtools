# tests/testthat/test-load_cv_sheets.R
library(testthat)
library(dplyr)
library(mockery)
# library(academicCVtools) # loaded by testthat

if(requireNamespace("googledrive", quietly = TRUE)) {
  googledrive::drive_deauth()
}

# --- Test Block 1: load with named list ---
test_that("load_cv_sheets works with a named list and returns correctly named list", {

  # 1. Inputs
  doc_id <- "test_doc_id_1"
  sheets_config_named <- list(
    "Sheet One" = "data_one",
    "Sheet 2" = "data_two"
  )
  # Expected data that .load_sheets_data should return (unnamed list)
  mock_data_sheet1 <- dplyr::tibble(a = 1)
  mock_data_sheet2 <- dplyr::tibble(b = "x")
  expected_unnamed_list <- list(mock_data_sheet1, mock_data_sheet2)

  # 2. Mocks
  stub(load_cv_sheets, ".resolve_doc_identifier", function(doc_identifier) doc_identifier)
  mock_loader <- mockery::mock(expected_unnamed_list)
  stub(load_cv_sheets, ".load_sheets_data", mock_loader)
  stub(load_cv_sheets, ".validate_load_cv_sheets_args", TRUE)
  stub(load_cv_sheets, ".prepare_sheet_load_config",
                list(sheet_names_to_read = c("Sheet One", "Sheet 2"),
                     target_list_names = c("data_one", "data_two")))

  # 3. Call
  result_list <- load_cv_sheets(
    doc_identifier = doc_id,
    sheets_to_load = sheets_config_named
  )

  # 4. Assertions
  expect_type(result_list, "list")
  expect_named(result_list, c("data_one", "data_two"))
  expect_equal(result_list$data_one, mock_data_sheet1)
  expect_equal(result_list$data_two, mock_data_sheet2)
  mockery::expect_called(mock_loader, 1)
})

# --- Test Block 2: load with unnamed vectorr ---
test_that("load_cv_sheets works with unnamed vector and uses janitor names", {

  # 1. Inputs
  doc_id <- "test_doc_id_2"
  sheets_config_unnamed <- c("Sheet One", "Sheet & Two")
  mock_data_sheet1 <- dplyr::tibble(c = 3)
  mock_data_sheet2 <- dplyr::tibble(d = "y")
  expected_unnamed_list <- list(mock_data_sheet1, mock_data_sheet2)
  expected_names <- c("sheet_one", "sheet_two")

  # 2. Mocks
  stub(load_cv_sheets, ".resolve_doc_identifier", function(doc_identifier) doc_identifier)
  mock_loader <- mockery::mock(expected_unnamed_list)
  stub(load_cv_sheets, ".load_sheets_data", mock_loader)
  stub(load_cv_sheets, ".validate_load_cv_sheets_args", TRUE)
  stub(load_cv_sheets, ".prepare_sheet_load_config",
                list(sheet_names_to_read = sheets_config_unnamed,
                     target_list_names = expected_names))

  # 3. Call
  suppressMessages({
    result_list <- load_cv_sheets(
      doc_identifier = doc_id,
      sheets_to_load = sheets_config_unnamed
    )
  })

  # 4. Assertions
  expect_type(result_list, "list")
  expect_named(result_list, expected_names)
  expect_equal(result_list$sheet_one, mock_data_sheet1)
  expect_equal(result_list$sheet_two, mock_data_sheet2)
  mockery::expect_called(mock_loader, 1)
})

# --- Test Block 3: Passing of ... arguments ---
test_that("load_cv_sheets passes ... arguments down to read_cv_sheet", {
  # 1. Inputs
  doc_id <- "test_doc_id_3"
  sheets_config <- list("Sheet1" = "s1")
  extra_arg_val <- "specific_na_string"

  # 2. Mocks
  mock_read_sheet <- mockery::mock(dplyr::tibble(a = 1), cycle = TRUE)

  # 3. Call within mocked environment
  testthat::with_mocked_bindings(
    {
      result <- load_cv_sheets(
        doc_identifier = doc_id,
        sheets_to_load = sheets_config,
        na = extra_arg_val # Argument for read_sheet
      )
    },
    # Define mocks for functions in the academicCVtools namespace
    .package = "academicCVtools",
    read_cv_sheet = mock_read_sheet,
    .resolve_doc_identifier = function(id) id,
    .validate_load_cv_sheets_args = function(...) TRUE,
    .prepare_sheet_load_config = function(...) {
      list(sheet_names_to_read = c("Sheet1"), target_list_names = c("s1"))
    }
  )

  # 4. Assertions
  mockery::expect_called(mock_read_sheet, 1)
  call_args <- mockery::mock_args(mock_read_sheet)[[1]]

  expect_equal(call_args$doc_identifier, doc_id)
  expect_equal(call_args$sheet_name, "Sheet1")
  expect_true("na" %in% names(call_args))
  expect_equal(call_args$na, extra_arg_val)
  expect_type(result, "list")
  expect_named(result, "s1")
  expect_equal(nrow(result$s1), 1)
})

# --- Test Block 4: Error in validation ---
test_that("load_cv_sheets stops if validation fails", {
  invalid_sheets_config <- list("ValidSheet" = 123)
  expect_error(
    load_cv_sheets(doc_identifier = "any_id", sheets_to_load = invalid_sheets_config),
    regexp = "Assertion on 'sheets_to_load' failed"
  )
})

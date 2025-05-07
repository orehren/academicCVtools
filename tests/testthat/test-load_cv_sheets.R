# tests/testthat/test-load_cv_sheets.R
library(testthat)
library(dplyr)
library(mockery)
# library(academicCVtools) # loaded by testthat

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

  # 2. Mock the internal HELPER .load_sheets_data
  mock_loader <- mockery::mock(expected_unnamed_list) # Return the list directly
  mockery::stub(load_cv_sheets, ".load_sheets_data", mock_loader)

  # Mock validator and config preparer (optional, but helps isolate)
  mockery::stub(load_cv_sheets, ".validate_load_cv_sheets_args", TRUE)
  mockery::stub(load_cv_sheets, ".prepare_sheet_load_config",
                list(sheet_names_to_read = c("Sheet One", "Sheet 2"),
                     target_list_names = c("data_one", "data_two")))

  # 3. Call the function
  result_list <- load_cv_sheets(
    doc_identifier = doc_id,
    sheets_to_load = sheets_config_named
  )

  # 4. Assertions
  expect_type(result_list, "list")
  expect_named(result_list, c("data_one", "data_two"))
  expect_equal(result_list$data_one, mock_data_sheet1)
  expect_equal(result_list$data_two, mock_data_sheet2)
  # Check that the MOCKED loader was called once
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

  # 2. Mock the internal HELPER .load_sheets_data
  mock_loader <- mockery::mock(expected_unnamed_list)
  mockery::stub(load_cv_sheets, ".load_sheets_data", mock_loader)

  # Mock validator and config preparer
  mockery::stub(load_cv_sheets, ".validate_load_cv_sheets_args", TRUE)
  mockery::stub(load_cv_sheets, ".prepare_sheet_load_config",
                list(sheet_names_to_read = sheets_config_unnamed,
                     target_list_names = expected_names))

  # 3. Call the function
  suppressMessages({ # Suppress potential janitor message if not mocking .prepare...
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


# --- Test Block 3: Weitergabe von ... Argumenten ---
test_that("load_cv_sheets passes ... arguments correctly to the loader", {

  # 1. Inputs
  doc_id <- "test_doc_id_3"
  sheets_config <- list("Sheet1" = "s1")
  extra_arg_val <- "specific_na"

  # 2. Mock .load_sheets_data
  #    a) Erstelle einen Mock NUR zum Aufzeichnen der Argumente
  mock_recorder <- mockery::mock()

  #    b) Definiere die Funktion, die das stub zurückgeben soll
  stub_return_func <- function(sheet_names_to_read, doc_identifier, ...) {
    # Führe den Recorder-Mock aus, um Argumente zu speichern
    mock_recorder(sheet_names_to_read = sheet_names_to_read,
                  doc_identifier = doc_identifier,
                  ...)
    # Gib den erwarteten Wert zurück (Liste von NULLs)
    return(vector("list", length(sheet_names_to_read)))
  }

  #    c) Ersetze den Helper durch die Funktion, die den Recorder aufruft
  mockery::stub(load_cv_sheets, ".load_sheets_data", stub_return_func)

  # Mock validator and config preparer (remain the same)
  mockery::stub(load_cv_sheets, ".validate_load_cv_sheets_args", TRUE)
  mockery::stub(load_cv_sheets, ".prepare_sheet_load_config",
                list(sheet_names_to_read = c("Sheet1"), target_list_names = c("s1")))

  # 3. Call the function with an extra argument
  result_list_corrected <- load_cv_sheets(
    doc_identifier = doc_id,
    sheets_to_load = sheets_config,
    na_strings = extra_arg_val
  )

  # 4. Assertions
  # Check call count and arguments using the RECORDER mock
  mockery::expect_called(mock_recorder, 1) # Prüfe den Recorder
  call_args <- mockery::mock_args(mock_recorder)[[1]] # Hole Argumente vom Recorder

  expect_equal(call_args$doc_identifier, doc_id)
  expect_equal(call_args$sheet_names_to_read, c("Sheet1"))
  expect_true("na_strings" %in% names(call_args))
  expect_equal(call_args$na_strings, extra_arg_val)

  # Assertions on the result (remain the same)
  expect_type(result_list_corrected, "list")
  expect_named(result_list_corrected, "s1")
  expect_equal(length(result_list_corrected), 1)
  expect_null(result_list_corrected$s1)

}) # End Test Block 3

# --- Test Block 4: Error in validation ---
test_that("load_cv_sheets stops if validation fails", {
  # (Dieser Test bleibt unverändert, da er den Validator testet)
  invalid_sheets_config <- list("ValidSheet" = 123)

  expect_error(
    load_cv_sheets(doc_identifier = "any_id", sheets_to_load = invalid_sheets_config),
    # Match the specific checkmate error for list element types
    regexp = "Must be of type 'character', not 'double'"
  )
})

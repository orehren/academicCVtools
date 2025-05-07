# tests/testthat/test-read_cv_sheet.R
library(testthat)
library(dplyr)
# library(mockery) # mockery wird implizit von testthat geladen, wenn verfügbar? Sicherer ist, es zu laden.
library(mockery)

# --- Test Block 1 ---
test_that("read_cv_sheet successfully reads data using a unique name", {

  # 1. Define expected results and inputs
  sheet_name_to_find <- "MyDataSheet"
  doc_name_to_find <- "My Test Sheet"
  expected_data <- dplyr::tibble(ColA = c("A", "B"), ColB = c(1, 2))

  # Define what our HELPER functions should return
  # .resolve_doc_identifier should return a dummy dribble for the name case
  dummy_dribble <- structure(
    data.frame(name = doc_name_to_find, id = "dummy_id_123"),
    class = c("dribble", "data.frame") # Minimal dribble structure
  )
  # .check_sheet_existence should return TRUE (or not abort)
  # .read_sheet_data should return the expected data

  # 2. Mock OUR HELPER functions using mockery::stub
  # Mock the function that resolves the identifier
  mockery::stub(
    where = read_cv_sheet, # Mock inside the main function where helpers are called
    what = ".resolve_doc_identifier",
    how = function(...) { dummy_dribble } # Return the dummy dribble
  )

  # Mock the function that checks sheet existence (make it do nothing/return TRUE)
  mockery::stub(
    where = read_cv_sheet,
    what = ".check_sheet_existence",
    how = function(...) { invisible(TRUE) }
  )

  # Mock the function that reads the data
  mockery::stub(
    where = read_cv_sheet,
    what = ".read_sheet_data",
    how = function(...) { expected_data } # Return the expected tibble
  )

  # Mock the helper for post-read checks (optional, but safer)
  # It should just return the data it receives
  mockery::stub(
    where = read_cv_sheet,
    what = ".check_read_result",
    how = function(sheet_data, ...) { sheet_data }
  )

  # Mock as_id used only for messages within read_cv_sheet (if any direct calls remain)
  # It seems as_id is mainly called within the mocked helpers now, so this might be less critical
  # mockery::stub(read_cv_sheet, "googledrive::as_id", function(...) "dummy_id_for_msg")


  # 3. Call the function under test
  actual_data <- read_cv_sheet(
    doc_identifier = doc_name_to_find,
    sheet_name = sheet_name_to_find
  )

  # 4. Assert expectations
  expect_equal(actual_data, expected_data)
  expect_s3_class(actual_data, "tbl_df")

}) # End test_that block 1


# --- Test Block 2: read via ID ---
test_that("read_cv_sheet successfully reads data using a document ID", {

  # 1. Define expected results and inputs
  sheet_name_to_find <- "DataSheetByID"
  doc_id_to_use <- "a_valid_looking_google_sheet_id_string_44_chars"
  expected_data <- dplyr::tibble(X = c("ID_Data"), Y = c(100))

  # Define what our HELPER functions should return
  # .resolve_doc_identifier should return the ID string itself
  # .check_sheet_existence should return TRUE (or not abort)
  # .read_sheet_data should return the expected data

  # 2. Mock OUR HELPER functions
  # Mock the function that resolves the identifier
  mockery::stub(
    where = read_cv_sheet,
    what = ".resolve_doc_identifier",
    how = function(doc_identifier, ...) {
      # Check if the input matches the expected ID (optional sanity check)
      # testthat::expect_equal(doc_identifier, doc_id_to_use)
      return(doc_id_to_use) # Return the ID string
    }
  )

  # Mock the function that checks sheet existence (make it do nothing/return TRUE)
  mockery::stub(
    where = read_cv_sheet,
    what = ".check_sheet_existence",
    how = function(...) { invisible(TRUE) }
  )

  # Mock the function that reads the data
  mockery::stub(
    where = read_cv_sheet,
    what = ".read_sheet_data",
    how = function(...) { expected_data } # Return the expected tibble
  )

  # Mock the helper for post-read checks
  mockery::stub(
    where = read_cv_sheet,
    what = ".check_read_result",
    how = function(sheet_data, ...) { sheet_data }
  )

  # Mock as_id used only for messages (less critical now)
  mockery::stub(read_cv_sheet, "googledrive::as_id", function(...) "dummy_id_for_msg")


  # 3. Call the function under test
  actual_data <- read_cv_sheet(
    doc_identifier = doc_id_to_use,
    sheet_name = sheet_name_to_find
  )

  # 4. Assert expectations
  expect_equal(actual_data, expected_data)
  expect_s3_class(actual_data, "tbl_df")

}) # End test_that block 2


# --- Test Block 3: read via URL ---
test_that("read_cv_sheet successfully reads data using a document URL", {

  # 1. Define expected results and inputs
  sheet_name_to_find <- "DataSheetByURL"
  # Use a realistic looking URL structure
  doc_url_to_use <- "https://docs.google.com/spreadsheets/d/a_valid_looking_google_sheet_id_string_44_chars/edit#gid=0"
  # The expected ID extracted from the URL
  expected_id_from_url <- "a_valid_looking_google_sheet_id_string_44_chars"
  expected_data <- dplyr::tibble(URL_Col = c("DataFromURL"), Value = c(TRUE))

  # Define what our HELPER functions should return
  # .resolve_doc_identifier should return the extracted ID string
  # .check_sheet_existence should return TRUE (or not abort)
  # .read_sheet_data should return the expected data

  # 2. Mock OUR HELPER functions
  # Mock the function that resolves the identifier
  mockery::stub(
    where = read_cv_sheet,
    what = ".resolve_doc_identifier",
    how = function(doc_identifier, ...) {
      # Check if the input matches the expected URL (optional sanity check)
      # testthat::expect_equal(doc_identifier, doc_url_to_use)
      # Simulate successful URL parsing returning the ID
      return(expected_id_from_url)
    }
  )

  # Mock the function that checks sheet existence
  mockery::stub(
    where = read_cv_sheet,
    what = ".check_sheet_existence",
    how = function(...) { invisible(TRUE) }
  )

  # Mock the function that reads the data
  mockery::stub(
    where = read_cv_sheet,
    what = ".read_sheet_data",
    how = function(...) { expected_data }
  )

  # Mock the helper for post-read checks
  mockery::stub(
    where = read_cv_sheet,
    what = ".check_read_result",
    how = function(sheet_data, ...) { sheet_data }
  )

  # Mock as_id used only for messages
  mockery::stub(read_cv_sheet, "googledrive::as_id", function(...) "dummy_id_for_msg")


  # 3. Call the function under test
  actual_data <- read_cv_sheet(
    doc_identifier = doc_url_to_use,
    sheet_name = sheet_name_to_find
  )

  # 4. Assert expectations
  expect_equal(actual_data, expected_data)
  expect_s3_class(actual_data, "tbl_df")

}) # End test_that block 3


# --- Test Block 4: Error - Sheet not found ---
test_that("read_cv_sheet throws error when sheet_name does not exist", {

  # 1. Define inputs
  sheet_name_not_existing <- "ThisSheetDoesNotExist"
  doc_id_to_use <- "another_dummy_sheet_id"

  # Define what the HELPER functions should return/do
  # .resolve_doc_identifier should succeed (return the ID)
  # .check_sheet_existence should ABORT with the specific error

  # 2. Mock OUR HELPER functions
  # Mock the identifier resolver to succeed
  mockery::stub(
    where = read_cv_sheet,
    what = ".resolve_doc_identifier",
    how = function(...) { doc_id_to_use }
  )

  # Mock the sheet existence check to throw the expected error
  mockery::stub(
    where = read_cv_sheet,
    what = ".check_sheet_existence",
    how = function(ss_input, sheet_name, ...) {
      # Simulate the abort that happens inside .check_sheet_existence
      # We need to match the expected error message structure
      # Note: We don't need the real .create_available_sheets_message here,
      # just the structure of the cli_abort call.
      cli::cli_abort(
        c("Sheet named {.val {sheet_name}} not found in document*", # Use * for partial match
          "i" = "Available sheets: *"), # Use * for partial match
        # Match the class potentially set by cli_abort
        # class = "some_error_class_if_known"
        call. = FALSE # Match the call. = FALSE used in the helper
      )
    }
  )

  # Mocks for .read_sheet_data and .check_read_result are not strictly
  # necessary here, as the function should abort before calling them.
  # However, adding them can prevent unexpected errors if the logic changes.
  mockery::stub(read_cv_sheet, ".read_sheet_data", function(...) { NULL })
  mockery::stub(read_cv_sheet, ".check_read_result", function(d, ...) { d })


  # 3. Assert that the expected error is thrown
  expect_error(
    read_cv_sheet(
      doc_identifier = doc_id_to_use,
      sheet_name = sheet_name_not_existing
    ),
    # We can match the error message partially using regexp
    regexp = "Sheet named.*not found"
    # Alternatively, match the error class if cli sets one consistently
    # class = "some_error_class"
  )

}) # End test_that block 4


# --- Test Block 5: Error - Name not unique ---
test_that("read_cv_sheet throws error when doc_identifier name is ambiguous", {

  # 1. Define inputs
  ambiguous_doc_name <- "Duplicate Sheet Name"
  sheet_name_to_find <- "SomeSheet"

  # Define what the HELPER functions should return/do
  # .resolve_doc_identifier should ABORT with the specific ambiguity error

  # 2. Mock OUR HELPER functions
  # Mock the identifier resolver to simulate the ambiguity error
  mockery::stub(
    where = read_cv_sheet,
    what = ".resolve_doc_identifier",
    how = function(doc_identifier, ...) {
      # Simulate the abort that happens inside .resolve_identifier_as_name
      # when nrow(found_files_dribble) > 1
      cli::cli_abort(
        c("Multiple Google Sheet documents found with the name {.val {doc_identifier}}.", # Match message
          "i" = "Please use the unique document ID or URL instead."),
        call. = FALSE # Match call. = FALSE
      )
    }
  )

  # Mocks for other helpers are not strictly needed as it should abort before them.
  mockery::stub(read_cv_sheet, ".check_sheet_existence", function(...) { invisible(TRUE) })
  mockery::stub(read_cv_sheet, ".read_sheet_data", function(...) { NULL })
  mockery::stub(read_cv_sheet, ".check_read_result", function(d, ...) { d })


  # 3. Assert that the expected error is thrown
  expect_error(
    read_cv_sheet(
      doc_identifier = ambiguous_doc_name,
      sheet_name = sheet_name_to_find
    ),
    # Match the error message partially
    regexp = "Multiple Google Sheet documents found"
  )

}) # End test_that block 5


# --- Test Block 6: Error - Document not found (Name) ---
test_that("read_cv_sheet throws error when doc_identifier name is not found", {

  # 1. Define inputs
  non_existent_doc_name <- "This Document Does Not Exist In Drive"
  sheet_name_to_find <- "AnySheet"

  # Define what the HELPER functions should return/do
  # .resolve_doc_identifier should ABORT with the specific "not found" error

  # 2. Mock OUR HELPER functions
  # Mock the identifier resolver to simulate the "not found" error
  mockery::stub(
    where = read_cv_sheet,
    what = ".resolve_doc_identifier",
    how = function(doc_identifier, ...) {
      # Simulate the abort that happens inside .resolve_identifier_as_name
      # when nrow(found_files_dribble) == 0
      cli::cli_abort(
        "No Google Sheet document found with the exact name {.val {doc_identifier}}.", # Match message
        call. = FALSE # Match call. = FALSE
      )
    }
  )

  # Mocks for other helpers are not strictly needed as it should abort before them.
  mockery::stub(read_cv_sheet, ".check_sheet_existence", function(...) { invisible(TRUE) })
  mockery::stub(read_cv_sheet, ".read_sheet_data", function(...) { NULL })
  mockery::stub(read_cv_sheet, ".check_read_result", function(d, ...) { d })


  # 3. Assert that the expected error is thrown
  expect_error(
    read_cv_sheet(
      doc_identifier = non_existent_doc_name,
      sheet_name = sheet_name_to_find
    ),
    # Match the error message partially
    regexp = "No Google Sheet document found"
  )

}) # End test_that block 6


# --- Test Block 7: Error - Document not found / accessible (ID / URL) ---
test_that("read_cv_sheet throws error when ID/URL is invalid/inaccessible", {

  # 1. Define inputs
  invalid_or_inaccessible_id <- "invalid_or_no_access_dummy_id"
  sheet_name_to_find <- "AnySheet"

  # Define what the HELPER functions should return/do
  # .resolve_doc_identifier should succeed (return the ID)
  # .check_sheet_existence should simulate gs4_get failure by returning NULL
  # .read_sheet_data should then be called with the bad ID and throw an error

  # 2. Mock OUR HELPER functions
  # Mock the identifier resolver to succeed
  mockery::stub(
    where = read_cv_sheet,
    what = ".resolve_doc_identifier",
    how = function(...) { invalid_or_inaccessible_id }
  )

  # Mock the sheet existence check to simulate gs4_get failure (returns NULL)
  # The cli_inform message inside the real function will still be printed
  mockery::stub(
    where = read_cv_sheet,
    what = ".check_sheet_existence",
    how = function(...) {
      # Simulate the tryCatch returning NULL when gs4_get fails
      return(NULL)
      # We don't need to simulate the cli_inform here, just the return value
    }
  )

  # Mock .read_sheet_data to simulate the error that read_sheet would throw
  # when called with an invalid ID/ss_input after gs4_get failed.
  # The exact error message might vary, so we use a general pattern.
  mockery::stub(
    where = read_cv_sheet,
    what = ".read_sheet_data",
    how = function(ss_input, sheet_name, ...) {
      # Simulate the abort from the tryCatch inside .read_sheet_data
      cli::cli_abort(
        c("Failed to read sheet {.val {sheet_name}} from document*", # Partial match
          "x" = "Original error:.*"), # Match any original error
        call. = FALSE
      )
    }
  )

  # Mock post-read check (won't be reached)
  mockery::stub(read_cv_sheet, ".check_read_result", function(d, ...) { d })
  # Mock as_id used for messages
  mockery::stub(read_cv_sheet, "googledrive::as_id", function(...) invalid_or_inaccessible_id)


  # 3. Assert that the expected error is thrown
  expect_error(
    read_cv_sheet(
      doc_identifier = invalid_or_inaccessible_id,
      sheet_name = sheet_name_to_find
    ),
    # Match the error message from the mocked .read_sheet_data
    regexp = "Failed to read sheet.*from document"
  )

}) # End test_that block 7



# --- Test Block 8: Error - Read error (read_sheet fails) ---
test_that("read_cv_sheet throws error when read_sheet itself fails", {

  # 1. Define inputs
  doc_id_to_use <- "readable_doc_id_but_sheet_fails"
  sheet_name_to_find <- "SheetThatCausesReadError"

  # Define what the HELPER functions should return/do
  # .resolve_doc_identifier should succeed
  # .check_sheet_existence should succeed
  # .read_sheet_data should ABORT with a simulated read error

  # 2. Mock OUR HELPER functions
  # Mock the identifier resolver to succeed
  mockery::stub(
    where = read_cv_sheet,
    what = ".resolve_doc_identifier",
    how = function(...) { doc_id_to_use }
  )

  # Mock the sheet existence check to succeed
  mockery::stub(
    where = read_cv_sheet,
    what = ".check_sheet_existence",
    how = function(...) { invisible(TRUE) }
  )

  # Mock .read_sheet_data to simulate the error from read_sheet
  mockery::stub(
    where = read_cv_sheet,
    what = ".read_sheet_data",
    how = function(ss_input, sheet_name, ...) {
      # Simulate the abort from the tryCatch inside .read_sheet_data
      cli::cli_abort(
        c("Failed to read sheet {.val {sheet_name}} from document*", # Partial match
          "x" = "Original error: Some API error during read (e.g., 403 Forbidden)"), # Simulate specific error
        call. = FALSE
      )
    }
  )

  # Mock post-read check (won't be reached)
  mockery::stub(read_cv_sheet, ".check_read_result", function(d, ...) { d })
  # Mock as_id used for messages
  mockery::stub(read_cv_sheet, "googledrive::as_id", function(...) doc_id_to_use)


  # 3. Assert that the expected error is thrown
  expect_error(
    read_cv_sheet(
      doc_identifier = doc_id_to_use,
      sheet_name = sheet_name_to_find
    ),
    # Match the error message from the mocked .read_sheet_data
    regexp = "Failed to read sheet.*Some API error"
  )

}) # End test_that block 8


# --- Test Block 9: Warning - Empty Sheet ---
test_that("read_cv_sheet warns and returns empty tibble for empty sheet", {

  # 1. Define inputs
  doc_id_to_use <- "doc_with_empty_sheet"
  empty_sheet_name <- "ThisSheetIsEmpty"

  # Define what the HELPER functions should return/do
  # .resolve_doc_identifier should succeed
  # .check_sheet_existence should succeed
  # .read_sheet_data should return an EMPTY tibble (0 rows)
  # .check_read_result should issue a warning and return the empty tibble

  # Create an empty tibble structure (column names might vary or be absent)
  # If read_sheet returns 0 rows but HAS headers, it will have column names.
  # If the sheet is truly empty (no header), it might return a 0x0 tibble.
  # Let's assume it returns a 0-row tibble with some potential columns for this test.
  empty_tibble_data <- dplyr::tibble(Header1 = character(0), Header2 = character(0))

  # 2. Mock OUR HELPER functions
  # Mock the identifier resolver to succeed
  mockery::stub(read_cv_sheet, ".resolve_doc_identifier", function(...) { doc_id_to_use })

  # Mock the sheet existence check to succeed
  mockery::stub(read_cv_sheet, ".check_sheet_existence", function(...) { invisible(TRUE) })

  # Mock .read_sheet_data to return the empty tibble
  mockery::stub(
    where = read_cv_sheet,
    what = ".read_sheet_data",
    how = function(...) { empty_tibble_data }
  )

  # Mock .check_read_result - THIS is where the warning happens.
  # We need to let the *real* .check_read_result run to test its warning logic,
  # BUT we need to ensure it receives the correct doc_id for the message.
  # So, we only mock as_id here.
  mockery::stub(read_cv_sheet, "googledrive::as_id", function(...) doc_id_to_use)


  # 3. Assert that the expected warning is issued AND the correct data is returned

  # Use expect_warning to check for the warning message (partial match)
  # Use expect_equal to check the returned value
  expect_warning(
    actual_data <- read_cv_sheet(
      doc_identifier = doc_id_to_use,
      sheet_name = empty_sheet_name
    ),
    regexp = "has no data \\(0 rows\\)"
  )

  # Check that the returned data is indeed the empty tibble
  # We might need to be flexible with column names/types depending on read_sheet's behavior
  expect_equal(nrow(actual_data), 0)
  expect_s3_class(actual_data, "tbl_df")
  # Optionally check column names if they are predictable for an empty read
  # expect_equal(names(actual_data), names(empty_tibble_data))

}) # End test_that block 9


# --- Test Block 10: Error - invalid sheet_name (checkmate) ---
test_that("read_cv_sheet throws error for invalid sheet_name type", {

  # 1. Define inputs with invalid sheet_name
  doc_id_to_use <- "any_valid_doc_id"
  invalid_sheet_name <- 123 # Not a string

  # No mocking needed here, the error should happen in the validation step

  # 2. Assert that the expected error from checkmate is thrown
  expect_error(
    read_cv_sheet(
      doc_identifier = doc_id_to_use,
      sheet_name = invalid_sheet_name
    ),
    # Checkmate errors often mention the assertion that failed
    regexp = "Assertion on 'sheet_name' failed: Must be of type 'string'"
    # The exact message might vary slightly based on checkmate version
  )

}) # End test_that block 10

# --- Test Block 11: Fehler - Ungültiges trim_ws (checkmate) ---
test_that("read_cv_sheet throws error for invalid trim_ws value", {

  # 1. Define inputs with invalid trim_ws
  doc_id_to_use <- "any_valid_doc_id"
  sheet_name_to_find <- "AnySheet"
  invalid_trim_ws <- "TRUE" # String instead of logical

  # No mocking needed

  # 2. Assert that the expected error from checkmate is thrown
  expect_error(
    read_cv_sheet(
      doc_identifier = doc_id_to_use,
      sheet_name = sheet_name_to_find,
      trim_ws = invalid_trim_ws
    ),
    # Korrigiertes regexp
    regexp = "Assertion on 'trim_ws' failed: Must be of type 'logical', not 'character'."
  )

}) # End test_that block 11

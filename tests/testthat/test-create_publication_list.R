# tests/testthat/test-create_publication_list.R
library(testthat)
library(dplyr)
library(mockery)
# library(academicCVtools) # Wird von testthat geladen

# --- Test Block 1: Basic success case ---
test_that("create_publication_list generates correct Typst output for basic case", {

  # 1. Setup: Create temporary files and expected JSON structure

  # Dummy Bib content
  bib_content <- c(
    "@article{Test2023, author={First Author and Test Person}, title={Some Title}, year={2023}, journal={Journal A}}",
    "@inproceedings{Test2022, author={Test Person}, title={Conf Paper}, year={2022}, booktitle={Conf B}}"
  )
  # Dummy CSL content
  csl_content <- '<?xml version="1.0"?><style xmlns="http://purl.org/net/xbiblio/csl" version="1.0"><info><title>test</title><id>test</id><updated>2024-01-01</updated></info><bibliography><layout><text value="DUMMY"/></layout></bibliography></style>'

  # --- Create temp files using base R ---
  bib_file <- tempfile(fileext = ".bib")
  csl_file <- tempfile(fileext = ".csl")
  # Ensure cleanup even if errors occur within the test block
  on.exit({
    if (file.exists(bib_file)) unlink(bib_file)
    if (file.exists(csl_file)) unlink(csl_file)
  }, add = TRUE)
  writeLines(bib_content, bib_file)
  writeLines(csl_content, csl_file)
  # --- End temp file creation ---

  # Define the JSON string that the mocked pandoc call should return
  # IMPORTANT: Adapt this JSON string based on the *actual* expected output!
  mock_json_output <- '{
        "pandoc-api-version": [1, 23, 1],
        "meta": {
          "references": {
            "t": "MetaList",
            "c": [
              { "t": "MetaMap", "c": { "id": {"t": "MetaString", "c": "Test2023"}, "type": {"t": "MetaString", "c": "article-journal"}, "issued": {"t": "MetaString", "c": "2023"} } },
              { "t": "MetaMap", "c": { "id": {"t": "MetaString", "c": "Test2022"}, "type": {"t": "MetaString", "c": "paper-conference"}, "issued": {"t": "MetaString", "c": "2022"} } }
            ]
          }
        },
        "blocks": [
          { "t": "Div", "c": [ ["refs", [], []], [
              { "t": "Div", "c": [ ["ref-Test2023", [], []], [ {"t": "Para", "c": [ {"t": "Str", "c": "First Author & Test Person (2023). Some Title."} ]} ] ] },
              { "t": "Div", "c": [ ["ref-Test2022", [], []], [ {"t": "Para", "c": [ {"t": "Str", "c": "Test Person (2022). Conf Paper."} ]} ] ] }
          ]]}
        ]
      }'

  author_to_highlight <- "First Author"

  # 2. Mock internal calls
  mockery::stub(create_publication_list, ".call_pandoc_json", mock_json_output)
  mockery::stub(create_publication_list, ".validate_executable_found", "/usr/bin/pandoc")

  # 3. Call the function under test
  actual_typst_output <- create_publication_list(
    bib_file = bib_file,
    author_name = author_to_highlight,
    csl_file = csl_file,
    author_highlight_markup = "**%s**"
  )

  # 4. Assert expectations using snapshot testing
  testthat::expect_snapshot(cat(actual_typst_output))

}) # End test_that block 1


# --- Test Block 2: Custom group_order ---
test_that("create_publication_list respects group_order for sorting", {

  # 1. Setup: Create temporary files and expected JSON structure
  bib_content <- c(
    "@article{AuthA2023, author={Author A}, title={Article One}, year={2023}, journal={Journal X}}",
    "@inproceedings{AuthB2022, author={Author B}, title={Conference Paper One}, year={2022}, booktitle={Conf Y}}",
    "@book{AuthC2021, author={Author C}, title={Book Alpha}, year={2021}, publisher={Pub Z}}",
    "@misc{AuthD2024, author={Author D}, title={Misc Item}, year={2024}, note={Note}}"
  )
  csl_content <- '<?xml version="1.0"?><style xmlns="http://purl.org/net/xbiblio/csl" version="1.0"><info><title>test</title><id>test</id><updated>2024-01-01</updated></info><bibliography><layout><text variable="title"/> (<text variable="issued"/>)</layout></bibliography></style>' # Slightly different CSL for variety

  bib_file <- tempfile(fileext = ".bib")
  csl_file <- tempfile(fileext = ".csl")
  on.exit({
    if (file.exists(bib_file)) unlink(bib_file)
    if (file.exists(csl_file)) unlink(csl_file)
  }, add = TRUE)
  writeLines(bib_content, bib_file)
  writeLines(csl_content, csl_file)

  # Define the JSON string that the mocked pandoc call should return
  # Ensure keys and types match the bib_content
  mock_json_output <- '{
    "pandoc-api-version": [1, 23, 1],
    "meta": {
      "references": { "t": "MetaList", "c": [
          { "t": "MetaMap", "c": { "id": {"t": "MetaString", "c": "AuthA2023"}, "type": {"t": "MetaString", "c": "article-journal"}, "issued": {"t": "MetaString", "c": "2023"} } },
          { "t": "MetaMap", "c": { "id": {"t": "MetaString", "c": "AuthB2022"}, "type": {"t": "MetaString", "c": "paper-conference"}, "issued": {"t": "MetaString", "c": "2022"} } },
          { "t": "MetaMap", "c": { "id": {"t": "MetaString", "c": "AuthC2021"}, "type": {"t": "MetaString", "c": "book"}, "issued": {"t": "MetaString", "c": "2021"} } },
          { "t": "MetaMap", "c": { "id": {"t": "MetaString", "c": "AuthD2024"}, "type": {"t": "MetaString", "c": "misc"}, "issued": {"t": "MetaString", "c": "2024"} } }
        ]
      }
    },
    "blocks": [ { "t": "Div", "c": [ ["refs", [], []], [
          { "t": "Div", "c": [ ["ref-AuthA2023", [], []], [ {"t": "Para", "c": [ {"t": "Str", "c": "Article One (2023)"} ]} ] ] },
          { "t": "Div", "c": [ ["ref-AuthB2022", [], []], [ {"t": "Para", "c": [ {"t": "Str", "c": "Conference Paper One (2022)"} ]} ] ] },
          { "t": "Div", "c": [ ["ref-AuthC2021", [], []], [ {"t": "Para", "c": [ {"t": "Str", "c": "Book Alpha (2021)"} ]} ] ] },
          { "t": "Div", "c": [ ["ref-AuthD2024", [], []], [ {"t": "Para", "c": [ {"t": "Str", "c": "Misc Item (2024)"} ]} ] ] }
      ]]}
    ]
  }'

  # Define custom labels and order
  custom_labels <- c(
    book = "Monographs",
    article = "Journal Contributions",
    inproceedings = "Conference Talks",
    misc = "Other Publications" # Explicitly label misc
  )
  # Define the desired output order of labels
  # "Conference Talks" should come first, then "Journal Contributions", then "Monographs"
  # "Other Publications" (misc) is not in group_order, so it should come last, alphabetically sorted with any other unmentioned.
  custom_group_order <- c("Conference Talks", "Journal Contributions", "Monographs")

  # 2. Mock internal calls
  mockery::stub(create_publication_list, ".call_pandoc_json", mock_json_output)
  mockery::stub(create_publication_list, ".validate_executable_found", "/usr/bin/pandoc")

  # 3. Call the function under test
  actual_typst_output <- create_publication_list(
    bib_file = bib_file,
    author_name = "Any Author", # Highlighting not the focus of this test
    csl_file = csl_file,
    group_labels = custom_labels,
    default_label = "Uncategorized", # Should not be used if all types are in custom_labels
    group_order = custom_group_order,
    author_highlight_markup = "%s" # Simple pass-through
  )

  # 4. Assert expectations using snapshot testing
  # This will create a new section in the snapshot file.
  testthat::expect_snapshot(cat(actual_typst_output))

}) # End test_that block 2


# --- Test Block 3: Fehler - Ungültige bib_file (checkmate) ---
test_that("create_publication_list errors with non-existent bib_file", {

  # csl_file muss existieren, sonst scheitert der Test an dieser früheren Validierung
  csl_content <- '<?xml version="1.0"?><style xmlns="http://purl.org/net/xbiblio/csl" version="1.0"><info><title>test</title><id>test</id><updated>2024-01-01</updated></info><bibliography><layout><text value="DUMMY"/></layout></bibliography></style>'
  csl_file <- tempfile(fileext = ".csl")
  on.exit(unlink(csl_file), add = TRUE)
  writeLines(csl_content, csl_file)

  expect_error(
    create_publication_list(
      bib_file = "this_file_does_not_exist.bib",
      author_name = "Any Author",
      csl_file = csl_file
      # Andere Argumente können Defaults verwenden
    ),
    regexp = "Assertion on 'bib_file' failed: File does not exist"
    # Oder genauer, je nach checkmate-Version: "'this_file_does_not_exist.bib' does not exist."
  )
})

# --- Test Block 4: Fehler - Ungültiges group_labels Format (checkmate) ---
test_that("create_publication_list errors with invalid group_labels format", {

  bib_file <- tempfile(fileext = ".bib"); writeLines("@book{t,t={t},y={t}}", bib_file)
  csl_file <- tempfile(fileext = ".csl"); writeLines('<?xml version="1.0"?><style xmlns="http://purl.org/net/xbiblio/csl" version="1.0"><info><title>test</title><id>test</id><updated>2024-01-01</updated></info><bibliography><layout><text value="DUMMY"/></layout></bibliography></style>', csl_file)
  on.exit({ unlink(bib_file); unlink(csl_file) }, add = TRUE)

  expect_error(
    create_publication_list(
      bib_file = bib_file,
      author_name = "Any Author",
      csl_file = csl_file,
      group_labels = c(article = 123) # Wert ist keine Zeichenkette
    ),
    # Korrigiertes regexp: Passt auf den allgemeineren Teil der Meldung
    regexp = "Assertion on 'group_labels' failed: Must be of type 'character', not 'double'."
  )
})

# --- Test Block 5: Fehler - Ungültiges author_highlight_markup (checkmate) ---
test_that("create_publication_list errors with invalid author_highlight_markup", {
  bib_file <- tempfile(fileext = ".bib"); writeLines("@book{t,t={t},y={t}}", bib_file)
  csl_file <- tempfile(fileext = ".csl"); writeLines('<?xml version="1.0"?><style xmlns="http://purl.org/net/xbiblio/csl" version="1.0"><info><title>test</title><id>test</id><updated>2024-01-01</updated></info><bibliography><layout><text value="DUMMY"/></layout></bibliography></style>', csl_file)
  on.exit({ unlink(bib_file); unlink(csl_file) }, add = TRUE)

  expect_error(
    create_publication_list(
      bib_file = bib_file,
      author_name = "Any Author",
      csl_file = csl_file,
      author_highlight_markup = "no_placeholder"
    ),
    # Korrigiertes regexp
    regexp = "Assertion on 'author_highlight_markup' failed: Must comply to pattern '%s'."
  )
})


# --- Test Block 6: Fehler - Pandoc-Aufruf schlägt fehl ---
test_that("create_publication_list returns empty block if pandoc call fails", {

  # 1. Setup: Gültige bib und csl Dateien (werden für Validierung gebraucht)
  bib_file <- tempfile(fileext = ".bib"); writeLines("@book{t,t={t},y={t}}", bib_file)
  csl_file <- tempfile(fileext = ".csl"); writeLines('<?xml version="1.0"?><style xmlns="http://purl.org/net/xbiblio/csl" class="in-text" version="1.0"><info><title>test</title><id>test</id><updated>2024-01-01</updated></info><bibliography><layout><text value="DUMMY"/></layout></bibliography></style>', csl_file)
  on.exit({ unlink(bib_file); unlink(csl_file) }, add = TRUE)

  # Erwarteter leerer Output-String
  func_name <- "testfunc" # Verwende einen anderen Namen, um Konflikte zu vermeiden
  expected_empty_output <- sprintf("```{=typst}\n#%s(())\n```", func_name)

  # 2. Mock .call_pandoc_json to return NULL (simulating failure)
  mockery::stub(create_publication_list, ".call_pandoc_json", NULL)

  # Mock .validate_executable_found to succeed, so the main function reaches .call_pandoc_json
  mockery::stub(create_publication_list, ".validate_executable_found", "/usr/bin/pandoc")

  # 3. Call the function under test
  # Unterdrücke die Warnung, die .call_pandoc_json ausgeben würde
  suppressWarnings({
    actual_output <- create_publication_list(
      bib_file = bib_file,
      author_name = "Any Author",
      csl_file = csl_file,
      typst_func_name = func_name # Verwende den Test-Funktionsnamen
    )
  })

  # 4. Assert expectations
  expect_equal(actual_output, expected_empty_output)

}) # End test_that block 6


# tests/testthat/test-create_publication_list.R

# (Vorheriger Code: library calls, Test 1-6)
# ...

# --- Test Block 7: Fehler - Ungültiges JSON von Pandoc ---
test_that("create_publication_list errors if pandoc returns invalid JSON", {

  # 1. Setup: Gültige bib und csl Dateien (für Validierung)
  bib_file <- tempfile(fileext = ".bib"); writeLines("@book{t,t={t},y={t}}", bib_file)
  csl_file <- tempfile(fileext = ".csl"); writeLines('<?xml version="1.0"?><style xmlns="http://purl.org/net/xbiblio/csl" class="in-text" version="1.0"><info><title>test</title><id>test</id><updated>2024-01-01</updated></info><bibliography><layout><text value="DUMMY"/></layout></bibliography></style>', csl_file)
  on.exit({ unlink(bib_file); unlink(csl_file) }, add = TRUE)

  invalid_json_string <- "This is not valid JSON}"

  # 2. Mock .call_pandoc_json to return the invalid JSON string
  mockery::stub(create_publication_list, ".call_pandoc_json", invalid_json_string)

  # Mock .validate_executable_found to succeed
  mockery::stub(create_publication_list, ".validate_executable_found", "/usr/bin/pandoc")

  # 3. Assert that the expected error (from jsonlite via cli_abort) is thrown
  expect_error(
    create_publication_list(
      bib_file = bib_file,
      author_name = "Any Author",
      csl_file = csl_file
    ),
    # jsonlite's error messages can vary, so match a general part
    # The cli_abort in our code wraps this.
    regexp = "Failed to parse Pandoc JSON output.*lexical error"
    # "lexical error" ist ein typischer jsonlite Fehler bei ungültigem JSON
  )

}) # End test_that block 7


# --- Test Block 8: Verhalten bei unerwarteter JSON-Struktur ---
test_that("create_publication_list returns empty for unexpected JSON structure", {

  # 1. Setup (bleibt gleich)
  bib_file <- tempfile(fileext = ".bib"); writeLines("@book{t,t={t},y={t}}", bib_file)
  csl_file <- tempfile(fileext = ".csl"); writeLines('<?xml version="1.0"?><style xmlns="http://purl.org/net/xbiblio/csl" class="in-text" version="1.0"><info><title>test</title><id>test</id><updated>2024-01-01</updated></info><bibliography><layout><text value="DUMMY"/></layout></bibliography></style>', csl_file)
  on.exit({ unlink(bib_file); unlink(csl_file) }, add = TRUE)

  json_missing_meta <- '{"pandoc-api-version": [1, 23, 1], "blocks": []}'
  json_missing_meta_refs_c <- '{"pandoc-api-version": [1, 23, 1], "meta": { "references": {"t": "MetaList"} }, "blocks": []}'

  func_name <- "testfunc_struct"
  expected_empty_output <- sprintf("```{=typst}\n#%s(())\n```", func_name)

  mockery::stub(create_publication_list, ".validate_executable_found", "/usr/bin/pandoc")

  # --- Testfall 1: 'meta' fehlt komplett ---
  mockery::stub(create_publication_list, ".call_pandoc_json", json_missing_meta)

  # Rufe die Funktion auf und prüfe nur den Rückgabewert
  # Die Warnung sollte im Test-Log erscheinen.
  actual_output_1 <- create_publication_list(
    bib_file = bib_file, author_name = "Any", csl_file = csl_file, typst_func_name = func_name
  )
  expect_equal(actual_output_1, expected_empty_output,
               info = "Testfall 1 (meta fehlt): Rückgabewert nicht wie erwartet.")

  # --- Testfall 2: 'meta$references$c' fehlt ---
  mockery::stub(create_publication_list, ".call_pandoc_json", json_missing_meta_refs_c)

  actual_output_2 <- create_publication_list(
    bib_file = bib_file, author_name = "Any", csl_file = csl_file, typst_func_name = func_name
  )
  expect_equal(actual_output_2, expected_empty_output,
               info = "Testfall 2 (meta$references$c fehlt): Rückgabewert nicht wie erwartet.")

}) # End test_that block 8


# tests/testthat/test-create_publication_list.R

# (Vorheriger Code: library calls, Test 1-8)
# ...

# --- Test Block 9: Warnung - Keine Referenzen im JSON (meta$references$c ist leer) ---
test_that("create_publication_list warns and returns empty if meta$references$c is empty", {

  # 1. Setup
  bib_file <- tempfile(fileext = ".bib"); writeLines("@book{t,t={t},y={t}}", bib_file)
  csl_file <- tempfile(fileext = ".csl"); writeLines('<?xml version="1.0"?><style xmlns="http://purl.org/net/xbiblio/csl" class="in-text" version="1.0"><info><title>test</title><id>test</id><updated>2024-01-01</updated></info><bibliography><layout><text value="DUMMY"/></layout></bibliography></style>', csl_file)
  on.exit({ unlink(bib_file); unlink(csl_file) }, add = TRUE)

  # JSON mit korrekter Struktur, aber leerer 'c' Liste für Referenzen
  json_empty_references_c <- '{
    "pandoc-api-version": [1, 23, 1],
    "meta": {
      "references": {
        "t": "MetaList",
        "c": []
      }
    },
    "blocks": [ { "t": "Div", "c": [ ["refs", [], []], [] ]} ]
  }'
  # blocks kann auch minimal sein, da es nach der references-Prüfung kommt

  func_name <- "testfunc_empty_refs"
  expected_empty_output <- sprintf("```{=typst}\n#%s(())\n```", func_name)

  # 2. Mock .call_pandoc_json und .validate_executable_found
  mockery::stub(create_publication_list, ".validate_executable_found", "/usr/bin/pandoc")
  mockery::stub(create_publication_list, ".call_pandoc_json", json_empty_references_c)

  # 3. Call function and check return value
  # Die Warnung sollte im Test-Log erscheinen
  actual_output <- suppressWarnings({ # Unterdrücke Warnung für diesen Wert-Check
    create_publication_list(
      bib_file = bib_file, author_name = "Any", csl_file = csl_file, typst_func_name = func_name
    )
  })
  expect_equal(actual_output, expected_empty_output)

}) # End test_that block 9


# tests/testthat/test-create_publication_list.R

# (Vorheriger Code: library calls, Test 1-9)
# ...

# --- Test Block 10: Autor-Highlighting ---
test_that("create_publication_list correctly highlights the specified author", {

  # 1. Setup
  bib_content <- c(
    "@article{HLTest2023, author={Doe, Jane and Rehren, Oliver and Smith, John}, title={Highlight Test}, year={2023}, journal={Test J}}",
    "@book{HLTest2022, author={Rehren, Oliver}, title={Sole Author Book}, year={2022}, publisher={Pub Inc}}"
  )
  csl_content <- '<?xml version="1.0"?><style xmlns="http://purl.org/net/xbiblio/csl" class="in-text" version="1.0"><info><title>test</title><id>test</id><updated>2024-01-01</updated></info><bibliography><layout><text variable="author" suffix=". " /><text variable="title"/> (<text variable="issued"/>).</layout></bibliography></style>'

  bib_file <- tempfile(fileext = ".bib"); writeLines(bib_content, bib_file)
  csl_file <- tempfile(fileext = ".csl"); writeLines(csl_content, csl_file)
  on.exit({ unlink(bib_file); unlink(csl_file) }, add = TRUE)

  author_to_highlight <- "Rehren, Oliver" # Name wie er im formatierten String erscheint
  highlight_markup <- "#FOO[%s]#BAR"   # Ein auffälliges, eindeutiges Markup

  # Erwarteter JSON-Output (vereinfacht, fokussiert auf formatierte Strings)
  mock_json_output <- paste0('{
    "pandoc-api-version": [1, 23, 1],
    "meta": { "references": { "t": "MetaList", "c": [
          { "t": "MetaMap", "c": { "id": {"t": "MetaString", "c": "HLTest2023"}, "type": {"t": "MetaString", "c": "article-journal"}, "issued": {"t": "MetaString", "c": "2023"} } },
          { "t": "MetaMap", "c": { "id": {"t": "MetaString", "c": "HLTest2022"}, "type": {"t": "MetaString", "c": "book"}, "issued": {"t": "MetaString", "c": "2022"} } }
        ] }
    },
    "blocks": [ { "t": "Div", "c": [ ["refs", [], []], [
          { "t": "Div", "c": [ ["ref-HLTest2023", [], []], [ {"t": "Para", "c": [ {"t": "Str", "c": "Doe, J., Rehren, Oliver, & Smith, J. Highlight Test (2023)."} ]} ] ] },
          { "t": "Div", "c": [ ["ref-HLTest2022", [], []], [ {"t": "Para", "c": [ {"t": "Str", "c": "Rehren, Oliver. Sole Author Book (2022)."} ]} ] ] }
      ]]}
    ]
  }')

  # 2. Mock internal calls
  mockery::stub(create_publication_list, ".call_pandoc_json", mock_json_output)
  mockery::stub(create_publication_list, ".validate_executable_found", "/usr/bin/pandoc")

  # 3. Call the function
  actual_typst_output <- create_publication_list(
    bib_file = bib_file,
    author_name = author_to_highlight,
    csl_file = csl_file,
    author_highlight_markup = highlight_markup,
    # Verwende Standard-group_labels und typst_func_name
    group_labels = c(article = "Article", book = "Book") # Vereinfacht für diesen Test
  )

  # 4. Assertions: Prüfe, ob das Markup im Output enthalten ist
  # Erwarteter String für den ersten Eintrag
  expected_highlighted_string1 <- "#FOO[Rehren, Oliver]#BAR"
  # Erwarteter String für den zweiten Eintrag
  expected_highlighted_string2 <- "#FOO[Rehren, Oliver]#BAR"

  # Verwende expect_snapshot für den gesamten Output
  testthat::expect_snapshot(cat(actual_typst_output))

  # Zusätzlich (oder alternativ zu Snapshot) spezifischere String-Checks
  expect_true(stringr::str_detect(actual_typst_output, stringr::fixed(expected_highlighted_string1)),
              info = "Highlighting for first author occurrence failed.")
  expect_true(stringr::str_detect(actual_typst_output, stringr::fixed(expected_highlighted_string2)),
              info = "Highlighting for second author occurrence failed.")

}) # End test_that block 10

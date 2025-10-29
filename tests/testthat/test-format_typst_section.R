# tests/testthat/test-format_typst_section.R
library(testthat)
library(dplyr)
# library(academicCVtools) # Wird von testthat geladen
# Mockery nicht unbedingt nötig, da keine externen API-Calls direkt hier

# --- Beispieldaten für Tests ---
# (Dieselbe wie in den @examples der Funktion)
fx_example_data <- dplyr::tibble(
  Date = c("2023-01-01", "2022-05-15", "2021-11-30"),
  Title = c("Project Alpha", "Research Grant", "Initial Concept"),
  Location = c("Lab A", "University X", "Office B"),
  Description = c("Developed core module.", "Secured funding.", "Drafted proposal."),
  Detail_A = c("Used R and Python", "Wrote application", "Market research"),
  Detail_B = c("Met all deadlines", NA, "Competitor analysis"), # Enthält NA
  Status = c("Completed", "Ongoing", "Completed"),
  Notes = c("Successful deployment", "Reporting due Q4", NA) # Enthält NA
)

# --- Test Block 1: Basic functionality, default na_action ('omit') ---
test_that("format_typst_section works for basic case with default NA handling", {

  actual_output <- format_typst_section(
    data = fx_example_data,
    typst_func = "#cvEntry"
  )

  # Snapshot-Test für den gesamten Output
  expect_snapshot(cat(actual_output))

  # Spezifischere Prüfungen (optional, da Snapshot robust ist)
  # Erwartet, dass NA-Felder (Detail_B Zeile 2, Notes Zeile 3) weggelassen werden
  expect_false(grepl("Detail_B.+NA", actual_output) || grepl("Detail_B.+none", actual_output)) # Nicht als "NA" oder none
  expect_false(grepl("Notes.+NA", actual_output) || grepl("Notes.+none", actual_output))
  # Prüfen, ob ein bekannter Wert da ist
  expect_true(grepl(fixed('Title: "Project Alpha"'), actual_output))
})

# --- Test Block 2: Column combination ---
# --- Test Block 2: Column combination ---
test_that("format_typst_section correctly combines columns", {

  actual_output <- format_typst_section(
    data = fx_example_data,
    typst_func = "#projectItem",
    combine_cols = c(Detail_A, Detail_B),
    combine_as = "key_points",
    combine_prefix = "* ",
    combine_sep = " | "
  )

  # Hauptprüfung über Snapshot
  expect_snapshot(cat(actual_output))

  # Zusätzliche, robustere Prüfungen:
  # 1. Sind die Original-Detail-Spalten weg?
  expect_false(grepl(fixed('Detail_A:'), actual_output),
               info = "Original column Detail_A should be removed after combining.")
  expect_false(grepl(fixed('Detail_B:'), actual_output),
               info = "Original column Detail_B should be removed after combining.")

  # 2. Ist die neue Spalte 'key_points' vorhanden?
  expect_true(grepl(fixed('key_points:'), actual_output),
              info = "New combined column 'key_points' should be present.")

  # 3. Optionale, weniger fragile Inhaltsprüfung für die kombinierte Spalte (Beispiel für Zeile 2)
  #    Wir prüfen nur, ob der Kerninhalt da ist, ohne exakte Formatierung.
  #    Dies ist weniger anfällig als der exakte String-Vergleich.
  #    Der Snapshot ist hier die Hauptquelle der Wahrheit für die Formatierung.
  expect_true(grepl("Wrote application", actual_output) && !grepl("Met all deadlines", substring(actual_output, regexpr("Wrote application", actual_output) - 50, regexpr("Wrote application", actual_output) + 50) ),
              info = "Content check for combined column (row 2) failed.")

})


# --- Test Block 3: Column exclusion ---
test_that("format_typst_section correctly excludes columns", {

  actual_output <- format_typst_section(
    data = fx_example_data,
    typst_func = "#simpleEntry",
    exclude_cols = c(Status, Notes, Location) # Tidyselect: Explizite Namen
  )

  # Snapshot-Test für den gesamten Output
  expect_snapshot(cat(actual_output))

  # Spezifische Prüfungen
  expect_false(grepl(fixed('Status:'), actual_output),
               info = "Column 'Status' should be excluded.")
  expect_false(grepl(fixed('Notes:'), actual_output),
               info = "Column 'Notes' should be excluded.")
  expect_false(grepl(fixed('Location:'), actual_output),
               info = "Column 'Location' should be excluded.")
  # Prüfen, ob eine nicht ausgeschlossene Spalte noch da ist
  expect_true(grepl(fixed('Title:'), actual_output),
              info = "Column 'Title' should still be present.")
})


# --- Test Block 4: Combining AND Excluding columns ---
test_that("format_typst_section handles combining and excluding simultaneously", {

  actual_output <- format_typst_section(
    data = fx_example_data,
    typst_func = "#focusedItem",
    combine_cols = starts_with("Detail_"), # Kombiniere Detail_A, Detail_B
    combine_as = "key_info",
    exclude_cols = c(Location, Description, Notes) # Schließe diese zusätzlich aus
  )

  expect_snapshot(cat(actual_output))

  # Prüfungen
  expect_true(grepl(fixed('key_info:'), actual_output),
              info = "Combined column 'key_info' should be present.")
  expect_false(grepl(fixed('Detail_A:'), actual_output),
               info = "Original 'Detail_A' should be removed.")
  expect_false(grepl(fixed('Location:'), actual_output),
               info = "Column 'Location' should be excluded.")
  expect_false(grepl(fixed('Description:'), actual_output),
               info = "Column 'Description' should be excluded.")
  expect_false(grepl(fixed('Notes:'), actual_output),
               info = "Column 'Notes' should be excluded.")
  # Prüfen, ob eine nicht berührte Spalte noch da ist
  expect_true(grepl(fixed('Date:'), actual_output),
              info = "Column 'Date' should still be present.")
})


# --- Test Block 5: Different na_action options ---
test_that("format_typst_section handles na_action correctly", {

  # Test na_action = "keep"
  output_na_keep <- format_typst_section(
    data = fx_example_data,
    typst_func = "#itemWithNone",
    na_action = "keep"
  )
  expect_snapshot(cat(output_na_keep)) # 'name' Argument entfernt
  expect_true(grepl(fixed('Detail_B: none'), output_na_keep), info = "NA in Detail_B (row 2) should be 'none'.")
  expect_true(grepl(fixed('Notes: none'), output_na_keep), info = "NA in Notes (row 3) should be 'none'.")

  # Test na_action = "string"
  output_na_string <- format_typst_section(
    data = fx_example_data,
    typst_func = "#itemWithStringNA",
    na_action = "string"
  )
  expect_snapshot(cat(output_na_string)) # 'name' Argument entfernt
  expect_true(grepl(fixed('Detail_B: "NA"'), output_na_string), info = "NA in Detail_B (row 2) should be '\"NA\"'.")
  expect_true(grepl(fixed('Notes: "NA"'), output_na_string), info = "NA in Notes (row 3) should be '\"NA\"'.")

  # Test na_action = "omit"
  output_na_omit <- format_typst_section(
    data = fx_example_data,
    typst_func = "#itemWithOmitNA",
    na_action = "omit"
  )
  expect_snapshot(cat(output_na_omit)) # 'name' Argument entfernt
  lines_omit <- strsplit(output_na_omit, "\n")[[1]]
  research_grant_line_omit <- lines_omit[grepl("Research Grant", lines_omit)]
  expect_false(grepl(fixed('"Detail_B":'), research_grant_line_omit),
               info = "NA in Detail_B (row 2) for 'Research Grant' line should be omitted.")
  initial_concept_line_omit <- lines_omit[grepl("Initial Concept", lines_omit)]
  expect_false(grepl(fixed('"Notes":'), initial_concept_line_omit),
               info = "NA in Notes (row 3) for 'Initial Concept' line should be omitted.")
})

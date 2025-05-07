# academicCVtools 0.1.0

## Initial Release

*   Initial public release of `academicCVtools`.
*   Provides core functionality for reading CV data from Google Sheets (`read_cv_sheet`, `load_cv_sheets`).
*   Includes function to process BibTeX files into formatted publication lists for Typst (`create_publication_list`) using Pandoc.
*   Offers a flexible function to transform R data frames into Typst array-of-dictionary structures (`format_typst_section`) with options for column combination and exclusion.
*   Basic argument validation using `checkmate` implemented.
*   Internal helper functions structured for maintainability.
*   Unit tests for core functions established using `testthat` and `mockery`.
*   Initial "Getting Started" vignette created.

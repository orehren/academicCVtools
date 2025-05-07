#' academicCVtools: Tools to Generate CV Sections from Data for Typst/Quarto
#'
#' The `academicCVtools` package provides a suite of functions to facilitate
#' the creation of academic Curricula Vitae (CVs) using a data-driven workflow.
#' It primarily focuses on reading structured data from sources like Google Sheets
#' and BibTeX files, processing this data, and then formatting it into Typst
#' function calls. These Typst calls can be seamlessly integrated into Quarto
#' documents that utilize a Typst template for rendering the final CV PDF.
#'
#' Key functionalities include:
#' \itemize{
#'   \item Reading specific sheets from Google Sheets documents, identified by ID, URL, or name
#'     ([read_cv_sheet()], [load_cv_sheets()]).
#'   \item Processing BibTeX files to generate formatted publication lists, with
#'     options for author highlighting, custom grouping, and sorting
#'     ([create_publication_list()]).
#'   \item Transforming R data frames (tibbles) into Typst array-of-dictionary
#'     structures, suitable for Typst function arguments, with flexible column
#'     combination and exclusion ([format_typst_section()]).
#' }
#'
#' The package aims to simplify the maintenance of CV content by separating it
#' from the presentation layer, allowing for easy updates and flexible output
#' formatting through Typst templates in a Quarto environment.
#'
#' @docType package
#' @name academicCVtools-package
#' @aliases academicCVtools
#' @keywords internal
"_PACKAGE"

# Suppress R CMD check NOTES about "no visible binding for global variable ‘.’"
# when using dplyr pipes. This is a common way to handle it.
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("."))
}

## usethis namespace: start
## Add imports for packages used throughout (if not handled by specific function imports)
## For example, if dplyr pipe is used widely, it's often imported here once.
## However, we've focused on function-level imports via roxygen2, which is preferred.
## This section is mainly for explicit `import()` or `importFrom()` in NAMESPACE
## if not managed by roxygen2 blocks elsewhere.
## usethis namespace: end
NULL

# academicCVtools

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`academicCVtools` is an R package designed to simplify the creation of academic Curricula Vitae (CVs) using Quarto and Typst. It reads structured data (e.g., publications, work experience, education) from Google Sheets and transforms it into Typst function calls, which can be directly used within a Quarto document that utilizes a corresponding Typst template.

## Features

*   Reads data from specific sheets (tabs) within a Google Sheets document.
*   Identifies the target document via its ID, URL, or name (performs a search when a name is provided).
*   Allows loading multiple sheets into a named list with a single function call (`load_cv_sheets`).
*   Transforms the data from a sheet (tibble) into a Typst string (`format_typst_section`).
*   Supports combining and excluding columns during the transformation.
*   Flexible handling of `NA` values.

## Installation

Currently, the package is under development. You can install it directly from GitHub (replace `yourusername/academicCVtools` with the actual path once it's on GitHub):

```r
	# install.packages("devtools") # If not already installed
	# devtools::install_github("orehren/academicCVtools") 
	# Or using renv:
	# renv::install("github::orehren/academicCVtools")
```

Once the package is available on CRAN, you will be able to install it using `install.packages("academicCVtools")`.

## Required Authentication

The package interacts with Google Drive and Google Sheets. You need to authenticate before using its functions. This is handled via the `googlesheets4` package. Typically, you run this once per session (or let `googlesheets4` cache a token):

```r
library(googlesheets4)
gs4_auth() 
# Or for non-interactive use (e.g., in GitHub Actions):
# gs4_auth(path = "/path/to/your/service-account-key.json")
```

Consult the documentation of `googlesheets4` and `gargle` for details on authentication methods.

## Basic Usage (Workflow Sketch)

1.  **Authenticate:** (See above)

2.  **Define Sheets:** Specify which sheets to load and what the resulting list elements should be named.

```r
    library(academicCVtools)
    
    my_doc <- "YOUR_SHEET_ID_OR_URL_OR_NAME" 
    sheets_config <- list(
      "Working Experiences" = "work", # Sheet Name -> Desired List Name
      "Education" = "edu",
      "Publications" = "pubs"
    )
```

3.  **Load Data:**

```r
    cv_data_list <- load_cv_sheets(doc_identifier = my_doc, sheets_to_load = sheets_config)
```

4.  **Use in Quarto:** Within a `.qmd` document targeting `typst` format:
    
```qmd
    ---
    title: "My Academic CV"
    format: typst
    # Other options...
    ---
    
    ```{r setup}
	    # Authentication, loading data, etc. (see above)
	    library(academicCVtools)
	    # ... load_cv_sheets call ...
	    ```
    
    ## Working Experiences
    
	    ```{r}
	    #| output: asis
	    cat(format_typst_section(
	    data = cv_data_list$work, 
	    typst_func = "#cvWorkEntry", 
	    combine_cols = starts_with("detail"),
	    combine_as = "key_points"
	    ))
	    ```
    
    ## Education
    
	    ```{r}
	    #| output: asis
	    cat(format_typst_section(
	    data = cv_data_list$edu, 
	    typst_func = "#cvEducationEntry"
	    # Potentially other options
	    ))
	    ```
    
    <!-- Repeat for other sections -->
    
```

(This assumes you have defined Typst functions like `#cvWorkEntry`, `#cvEducationEntry` in your template that can process the provided dictionaries.)

## License

The R code in this package is licensed under the GPL-3. See the `LICENSE` file.
The Typst template files (once included) will be licensed under CC BY-NC-SA 4.0. See the `LICENSE.note` file.

## Contributing

Please note that this project is currently under active development. If you have suggestions or encounter bugs, please feel free to open an issue on the GitHub repository (link to be added).

## Bug Reports

Please report any bugs or issues via GitHub Issues (add link once available).

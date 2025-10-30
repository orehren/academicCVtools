# create_publication_list generates correct Typst output for basic case

    Code
      cat(actual_typst_output)
    Output
      ```{=typst}
      #publication-list((
        (label: "Conference Paper", item: "Test Person (2022). Conf Paper."),
        (label: "Journal Article", item: "**First Author** & Test Person (2023). Some Title.")
      ))
      ```

# create_publication_list respects group_order for sorting

    Code
      cat(actual_typst_output)
    Output
      ```{=typst}
      #publication-list((
        (label: "Conference Talks", item: "Conference Paper One (2022)"),
        (label: "Journal Contributions", item: "Article One (2023)"),
        (label: "Monographs", item: "Book Alpha (2021)"),
        (label: "Other Publications", item: "Misc Item (2024)")
      ))
      ```

# create_publication_list correctly highlights the specified author

    Code
      cat(actual_typst_output)
    Output
      ```{=typst}
      #publication-list((
        (label: "Article", item: "Doe, J., #FOO[Rehren, Oliver]#BAR, & Smith, J. Highlight Test (2023)."),
        (label: "Book", item: "#FOO[Rehren, Oliver]#BAR. Sole Author Book (2022).")
      ))
      ```


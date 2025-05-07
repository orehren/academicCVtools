# format_typst_section works for basic case with default NA handling

    Code
      cat(actual_output)
    Output
      ```{typst}
      #cvEntry("Date": "2023-01-01", "Title": "Project Alpha", "Location": "Lab A", "Description": "Developed core module.", "Detail_A": "Used R and Python", "Detail_B": "Met all deadlines", "Status": "Completed", "Notes": "Successful deployment")
      #cvEntry("Date": "2022-05-15", "Title": "Research Grant", "Location": "University X", "Description": "Secured funding.", "Detail_A": "Wrote application", "Status": "Ongoing", "Notes": "Reporting due Q4")
      #cvEntry("Date": "2021-11-30", "Title": "Initial Concept", "Location": "Office B", "Description": "Drafted proposal.", "Detail_A": "Market research", "Detail_B": "Competitor analysis", "Status": "Completed")
      ```

# format_typst_section correctly combines columns

    Code
      cat(actual_output)
    Output
      ```{typst}
      #projectItem("Date": "2023-01-01", "Title": "Project Alpha", "Location": "Lab A", "Description": "Developed core module.", "Status": "Completed", "Notes": "Successful deployment", "key_points": "* Used R and Python | * Met all deadlines")
      #projectItem("Date": "2022-05-15", "Title": "Research Grant", "Location": "University X", "Description": "Secured funding.", "Status": "Ongoing", "Notes": "Reporting due Q4", "key_points": "* Wrote application")
      #projectItem("Date": "2021-11-30", "Title": "Initial Concept", "Location": "Office B", "Description": "Drafted proposal.", "Status": "Completed", "key_points": "* Market research | * Competitor analysis")
      ```

# format_typst_section correctly excludes columns

    Code
      cat(actual_output)
    Output
      ```{typst}
      #simpleEntry("Date": "2023-01-01", "Title": "Project Alpha", "Description": "Developed core module.", "Detail_A": "Used R and Python", "Detail_B": "Met all deadlines")
      #simpleEntry("Date": "2022-05-15", "Title": "Research Grant", "Description": "Secured funding.", "Detail_A": "Wrote application")
      #simpleEntry("Date": "2021-11-30", "Title": "Initial Concept", "Description": "Drafted proposal.", "Detail_A": "Market research", "Detail_B": "Competitor analysis")
      ```

# format_typst_section handles combining and excluding simultaneously

    Code
      cat(actual_output)
    Output
      ```{typst}
      #focusedItem("Date": "2023-01-01", "Title": "Project Alpha", "Status": "Completed", "key_info": "- Used R and Python\\n- Met all deadlines")
      #focusedItem("Date": "2022-05-15", "Title": "Research Grant", "Status": "Ongoing", "key_info": "- Wrote application")
      #focusedItem("Date": "2021-11-30", "Title": "Initial Concept", "Status": "Completed", "key_info": "- Market research\\n- Competitor analysis")
      ```

# format_typst_section handles na_action correctly

    Code
      cat(output_na_keep)
    Output
      ```{typst}
      #itemWithNone("Date": "2023-01-01", "Title": "Project Alpha", "Location": "Lab A", "Description": "Developed core module.", "Detail_A": "Used R and Python", "Detail_B": "Met all deadlines", "Status": "Completed", "Notes": "Successful deployment")
      #itemWithNone("Date": "2022-05-15", "Title": "Research Grant", "Location": "University X", "Description": "Secured funding.", "Detail_A": "Wrote application", "Detail_B": none, "Status": "Ongoing", "Notes": "Reporting due Q4")
      #itemWithNone("Date": "2021-11-30", "Title": "Initial Concept", "Location": "Office B", "Description": "Drafted proposal.", "Detail_A": "Market research", "Detail_B": "Competitor analysis", "Status": "Completed", "Notes": none)
      ```

---

    Code
      cat(output_na_string)
    Output
      ```{typst}
      #itemWithStringNA("Date": "2023-01-01", "Title": "Project Alpha", "Location": "Lab A", "Description": "Developed core module.", "Detail_A": "Used R and Python", "Detail_B": "Met all deadlines", "Status": "Completed", "Notes": "Successful deployment")
      #itemWithStringNA("Date": "2022-05-15", "Title": "Research Grant", "Location": "University X", "Description": "Secured funding.", "Detail_A": "Wrote application", "Detail_B": "NA", "Status": "Ongoing", "Notes": "Reporting due Q4")
      #itemWithStringNA("Date": "2021-11-30", "Title": "Initial Concept", "Location": "Office B", "Description": "Drafted proposal.", "Detail_A": "Market research", "Detail_B": "Competitor analysis", "Status": "Completed", "Notes": "NA")
      ```

---

    Code
      cat(output_na_omit)
    Output
      ```{typst}
      #itemWithOmitNA("Date": "2023-01-01", "Title": "Project Alpha", "Location": "Lab A", "Description": "Developed core module.", "Detail_A": "Used R and Python", "Detail_B": "Met all deadlines", "Status": "Completed", "Notes": "Successful deployment")
      #itemWithOmitNA("Date": "2022-05-15", "Title": "Research Grant", "Location": "University X", "Description": "Secured funding.", "Detail_A": "Wrote application", "Status": "Ongoing", "Notes": "Reporting due Q4")
      #itemWithOmitNA("Date": "2021-11-30", "Title": "Initial Concept", "Location": "Office B", "Description": "Drafted proposal.", "Detail_A": "Market research", "Detail_B": "Competitor analysis", "Status": "Completed")
      ```


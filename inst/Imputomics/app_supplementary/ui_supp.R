

ui_content_about <- function() {
  tagList(
    tags$footer(
      align = "right",
      style = "position:absolute; bottom:0; width:99%; height:30px; padding: 0px 0px 100px 100px;",
      HTML("<img src='funding.png' style='height: 100px'>"),
    ),
    fluidRow(
      column(2,
             align = "center",
             HTML("<img src='logo.png' height='140px'>"),
      ),
      column(10,
             h2("Welcome to Imputomics!"),
             h3("Improve your metabolomics analysis by addressing missing values with ease.")
      ),
    ),
    HTML('<hr style="border-color: black;">'),
    column(12,
           h3("About"),
           h5(HTML("This application incorporates R package <em>imputomics</em>. If you
            prefer to directly access R functions, the package is available"),
              a("on GitHub.", href = "https://github.com/BioGenies/imputomics")),
           h5("We appreciate your choice to use our application. If you have any
       questions, feedback, or suggestions, please submit them as an issue on
       GitHub. Your input is valuable to us!"),
           h4("Our app offers:"),
           h5(HTML("<b> 1. Seamless Integration:</b> Import datasets in various formats, such as Excel, CSV.")),
           h5(HTML("<b> 2. Visualize Missingness:</b> Gain insights into missing data patterns.")),
           h5(HTML("<b> 3. Customizable Imputation:</b> Choose from the largest set of imputation methods.")),
           h5(HTML("<b> 4. Performance Evaluation:</b> Compare imputation strategies for optimal results.")),
           h5(HTML("<b> 5. Export and Integration:</b> Export completed datasets and integrate with popular analysis platforms.")),
           h5(HTML("<b> 6. Secure and Confidential:</b> Your data privacy is our top priority.")),
           h4("Usage note"),
           h5("When selecting an imputation method, prioritize the structure of your data
       over the preconceived notions or your research hypothesis. The effectiveness of
       analytical methods varies based on dataset characteristics. Consider
       nuances like distribution, scale, missing value patterns, and relationships
       for a more accurate and reliable outcome, adhering to best practices in
       data analysis."),
           markdown(imputomics_citation()),
           markdown(imputomics_contact()),
           markdown(imputomics_funding()),
    )

  )
}


get_remove_html_content <- function(to_remove)
  HTML(ifelse(length(to_remove) > 0,
              paste0(paste(to_remove, collapse = ", "),
                     "<br/><br/> Total: ",
                     length(to_remove),
                     " variables."),
              "none"))


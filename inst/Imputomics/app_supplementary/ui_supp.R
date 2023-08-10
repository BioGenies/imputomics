

ui_content_about <- function() {
  tagList(
    fluidRow(
      column(1,
             HTML("<img src='logo.png' height='150px'>")
      ),
      column(10,
             h2("Welcome to Imputomics!"),
             h3("Improve your metabolomics analysis by addressing missing values with ease."),
      ),
    ),
    HTML('<hr style="border-color: black;">'),
    h3("Our app offers:"),
    h4(HTML("<b> 1. Seamless Integration:</b> Import datasets in various formats, such as Excel, CSV.")),
    h4(HTML("<b> 2. Visualize Missingness:</b> Gain insights into missing data patterns.")),
    h4(HTML("<b> 3. Customizable Imputation:</b> Choose from the largest set of imputation methods.")),
    h4(HTML("<b> 4. Performance Evaluation:</b> Compare imputation strategies for optimal results.")),
    h4(HTML("<b> 5. Export and Integration:</b> Export completed datasets and integrate with popular analysis platforms.")),
    h4(HTML("<b> 6. Secure and Confidential:</b> Your data privacy is our top priority.")),
    markdown(imputomics_citation()),
    markdown(imputomics_contact()),
    markdown(imputomics_funding()),
    markdown(imputomics_funding_images_shinyapp())
  )
}



ui_content_about <- function() {
  tagList(
    h2("Welcome to Imputomics!"),
    h3("Improve your metabolomics analysis by addressing missing values with ease."),
    HTML('<hr style="border-color: black;">'),
    h3("Our app offers:"),
    h4(HTML("<b> 1. Seamless Integration:</b> Import datasets in various formats, such as Excel, CSV.")),
    h4(HTML("<b> 2. Visualize Missingness:</b> Gain insights into missing data patterns.")),
    h4(HTML("<b> 3. Customizable Imputation:</b> Choose from the largest set of imputation methods.")),
    h4(HTML("<b> 4. Performance Evaluation:</b> Compare imputation strategies for optimal results.")),
    h4(HTML("<b> 5. Export and Integration:</b> Export completed datasets and integrate with popular analysis platforms.")),
    h4(HTML("<b> 6. Secure and Confidential:</b> Your data privacy is our top priority.")),
    br(),
    h3("Authors:"),
    h4("Jarosław Chilimoniuk, Krystyna Grzesiak, Jakub Kała, Dominik Nowakowski, Michał Burdukiewicz"),
    br(),
    h3("Citation:"),
    h4("Imputomics... tratatata citation"),
    br(),
    img(src='umb_logo.jpg', height="15%", width="15%", align="right")
  )
}



plot_mv_segment <- function(tmp_dat) {
  level_order <- tmp_dat %>%
    arrange(`% Missing`) %>%
    pull(Variable)

  tmp_dat %>%
    mutate() %>%
    ggplot(Variable = factor(Variable, levels = Variable)) +
    geom_segment(aes(y = Variable, yend = Variable,
                     x = 0, xend = `% Missing`),
                 size = 1.5) +
    geom_segment(aes(y = Variable, yend = Variable,
                     xend = 100, x = `% Missing`),
                 size = 1.5, col = "grey") +
    geom_point(mapping = aes(y = Variable, x = `% Missing`), size = 2) +
    geom_label(mapping = aes(y = Variable, x = `% Missing`,
                             label = paste0(`% Missing`, "%"))) +
    theme_minimal() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          title = element_text(size = 18)) +
    ggtitle("Percentage of missing values") +
    scale_y_discrete(limits = level_order) +
    xlab("% Missing")
}


plot_mv_heatmap <- function(tmp_dat) {

  gathercols <- colnames(tmp_dat)

  tmp_dat %>%
    mutate(Sample = 1:n()) %>%
    gather(Variable, Value, gathercols) %>%
    mutate(`Is missing` = is.na(Value)) %>%
    ggplot(aes(x = Sample, y = Variable, fill = `Is missing`)) +
    geom_tile() +
    scale_fill_manual(values = c("grey", "black")) +
    theme_minimal() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          title = element_text(size = 18)) +
    ggtitle("Missing values pattern")
}



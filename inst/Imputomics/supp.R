

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
    markdown(imputomics_citation()),
    markdown(imputomics_contact()),
    markdown(imputomics_funding()),
    markdown(imputomics_funding_images())
  )
}

validate_data <- function(uploaded_data, session, input) {

  # check if data is empty
  if(is.null(uploaded_data)){
    sendSweetAlert(session = session,
                   title = "No data!",
                   text = "Make sure that the uploaded file contains dataset  with numeric columns.",
                   type = "error")
    uploaded_data <- data.frame()
  } else {

    if(input[["NA_sign"]] == "zero") uploaded_data[raw_data == 0] <- NA

    # check if the columns are numeric
    if(any(!sapply(uploaded_data, is.numeric))) {
      uploaded_data <- uploaded_data[, sapply(uploaded_data, is.numeric)]

      if(ncol(uploaded_data) > 0) {
        showNotification("Your data contains non-numeric columns!
                       We will ignore them!",
                         session = session,
                         type = "warning")
      }else {
        sendSweetAlert(session = session,
                       title = "No numeric columns!",
                       text = "Make sure that the uploaded file contains dataset with numeric columns.",
                       type = "error")
        uploaded_data <- data.frame()
      }
    }

    if(sum(sapply(uploaded_data, is.numeric)) < 5)
      showNotification("You provided data with less than 5 numeric columns.
                             Some methods may not work properly.",
                       session = session,
                       type = "error")

  }

  uploaded_data
}



plot_mv_segment <- function(tmp_dat) {

  level_order <- tmp_dat %>%
    arrange(`% Missing`) %>%
    pull(Variable)

  tmp_dat <- tmp_dat %>%
    mutate(above_limit = `% Missing` > 20,
           colors_legend = ifelse(above_limit, "tomato", "black"))

  tmp_dat %>%
    ggplot(Variable = factor(Variable, levels = Variable)) +
    geom_segment(aes(y = Variable, yend = Variable,
                     x = 0, xend = `% Missing`, col = above_limit),
                 size = 1.5) +
    scale_color_manual(values = sort(unique(pull(tmp_dat, colors_legend)))) +
    geom_segment(aes(y = Variable, yend = Variable,
                     xend = 100, x = `% Missing`),
                 size = 1.5, col = "grey") +
    geom_point(mapping = aes(y = Variable, x = `% Missing`), size = 2) +
    geom_label(mapping = aes(y = Variable, x = `% Missing`,
                             label = paste0(`% Missing`, "%"))) +
    theme_minimal() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          title = element_text(size = 18),
          legend.position = "none") +
    ggtitle("Percentage of missing values") +
    scale_y_discrete(limits = level_order) +
    xlab("% Missing")
}


plot_mv_heatmap <- function(tmp_dat) {

  gathercols <- colnames(tmp_dat)

  tmp_dat %>%
    mutate(Sample = 1:n()) %>%
    gather(Variable, Value, all_of(gathercols)) %>%
    mutate(`Is missing` = is.na(Value)) %>%
    ggplot(aes(x = Sample, y = as.factor(Variable), fill = `Is missing`)) +
    geom_tile() +
    ylab("Variable") +
    scale_fill_manual(values = c("grey", "black")) +
    theme_minimal() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          title = element_text(size = 18)) +
    ggtitle("Missing values pattern")
}



get_variables_table <- function(missing_data) {
  mv_summary <- data.frame(
    Variable = colnames(missing_data),
    Percentage_Missing = 100*colMeans(is.na(missing_data))
  )%>%
    arrange(-Percentage_Missing) %>%
    rename(`% Missing` = Percentage_Missing)

  variables_table <- mv_summary %>%
    mutate(missing = `% Missing` > 0) %>%
    mutate(missing = ifelse(missing,
                            "missing variables",
                            "complete variables")) %>%
    group_by(missing) %>%
    summarise(n = n())

  list(mv_summary = mv_summary,
       variables_table = variables_table)
}


get_methods_table <- function(path = "methods_table.RDS") {
  methods_table <- readRDS(path) %>%
    mutate(name = paste0(name, " (", full_name, ")"))

  funs_imputomics <- ls("package:imputomics")
  funs_imputomics <- funs_imputomics[grepl("impute_", funs_imputomics)]

  methods_table %>%
    filter(imputomics_name %in% funs_imputomics)
}


plot_points_density <- function(dat, input) {

  method <- dat[["results"]][["success"]] %>%
    filter(name %in% input[["plot_methods"]]) %>%
    pull(imputomics_name)

  res <- dat[["results"]][["results"]]

  res_var <- res[[method]][, input[["plot_var"]]]
  miss_var <- dat[["missing_data"]][, input[["plot_var"]]]

  plt_dat <- data.frame(var = res_var,
                        missing_var = miss_var)

  points_plt <- plt_dat %>%
    mutate(imputed = is.na(missing_var)) %>%
    ggplot() +
    geom_quasirandom(aes(y = var, x = imputed, col = imputed)) +
    theme_minimal() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          axis.text.x = element_blank(),
          title = element_text(size = 18),
          legend.position = "bottom") +
    xlab("") +
    ylab(input[["plot_var"]]) +
    scale_color_manual(values=c("#3d3b3c", "#fb5607"))

density_plt_dat <- plt_dat %>%
  mutate(imputed = is.na(missing_var),
         missing = ifelse(imputed, var, NA),
         observed = ifelse(imputed, NA, var))

if(length(unique(density_plt_dat[["missing"]])) < 5) {
  dens_plt <- plt_dat %>%
    mutate(imputed = is.na(missing_var),
           missing = ifelse(imputed, var, NA),
           observed = ifelse(imputed, NA, var)) %>%
    ggplot() +
    geom_density(aes(x = observed), fill = "#3d3b3c", alpha = 0.4) +
    geom_histogram(aes(x = missing, y = ..density..), alpha = 0.4,
                   fill = "#fb5607", col = "#fb5607") +
    theme_minimal() +
    coord_flip() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          axis.title.x =  element_blank(),
          axis.text.y = element_blank(),
          title = element_text(size = 18),
          legend.position = "none") +
    xlab("")
} else {
  dens_plt <- plt_dat %>%
    mutate(imputed = is.na(missing_var),
           missing = ifelse(imputed, var, NA),
           observed = ifelse(imputed, NA, var)) %>%
    ggplot() +
    geom_density(aes(x = var, fill = imputed, col = imputed, alpha = 0.4)) +
    theme_minimal() +
    coord_flip() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          axis.title.x =  element_blank(),
          axis.text.y = element_blank(),
          title = element_text(size = 18),
          legend.position = "none") +
    xlab("") +
    scale_fill_manual(values=c("#3d3b3c", "#fb5607")) +
    scale_color_manual(values=c("#3d3b3c", "#fb5607"))
}

  points_plt + theme(legend.position = "right") + dens_plt +
    plot_layout(guides = "collect", design = "112") +
    plot_annotation(title = ggtitle(paste0("Variable: ",
                                           input[["plot_var"]],
                                           ",\nMethod: ",
                                           input[["plot_methods"]])),
                    caption = "Data imputed vs. observed",
                    theme = theme(plot.title = element_text(size = 16)))
}


save_excel <- function(dat, file, download_methods) {
  wb_file <- createWorkbook()
  addWorksheet(wb_file, "original_data")
  writeData(wb_file, "original_data",
            dat[["missing_data"]], colNames = TRUE)

  result_data <- dat[["results"]][["results"]]

  methods <- dat[["results"]][["success"]] %>%
    filter(name %in% download_methods) %>%
    pull(imputomics_name)

  result_data <- result_data[methods]
  methods <- str_replace_all(str_remove(names(result_data),"impute_"), "_", " ")
  for (i in 1:length(result_data)) {
    if(nrow(result_data[[i]]) != 0) {
      addWorksheet(wb_file, methods[i])
      writeData(wb_file, methods[i], result_data[[i]], colNames = TRUE)
    }
  }

  saveWorkbook(wb_file, file, overwrite = TRUE)
}

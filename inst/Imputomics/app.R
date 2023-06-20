library(shiny)
library(shinythemes)
library(DT)
library(readxl)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(tidyr)
library(imputomics)
library(shinycssloaders)
library(openxlsx)
library(ggbeeswarm)
library(stringr)

source("supp.R")

methods_table <- get_methods_table("methods_table.RDS")

ui <- navbarPage(
  theme = shinytheme("sandstone"),
  title = "Imputomics",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  tabPanel("About",
           ui_content_about()
  ),
  tabPanel("Load data",
           column(4,
                  style = 'border-right: 1px solid',
                  h2("Here you can upload your data!"),
                  br(),
                  h4("1. Select your metabolomics dataset in CSV or Excel:"),
                  fileInput(inputId = 'users_path',
                            label = "Upload file with missing values.",
                            multiple = FALSE,
                            accept = c(".csv", ".xlsx", ".rds")),
                  br(),
                  h4("2. Select missing value denotement:"),
                  selectInput("NA_sign",
                              "How is a missing value marked in your data?",
                              choices = c("zero", "NA"),
                              selected = "NA"),
                  br(),
                  h4("Upload example data."),
                  materialSwitch(inputId = "example_dat")
           ),
           column(6,
                  align = "center",
                  offset = 1,
                  h3("Dataset preview:"),
                  withSpinner(DT::dataTableOutput("missing_data"),
                              color = "black")
           )
  ),
  tabPanel("Visualization",
           column(2,
                  h4("Count Table:"),
                  withSpinner(tableOutput("mv_pctg"), color = "black"),
                  br(),
                  HTML('<hr style="border-color: black;">'),
                  br(),
                  radioButtons(inputId = "plot_type",
                               label = "Select plot:",
                               choices = c("Percentage",
                                           "Heatmap"),
                               inline = TRUE),
                  br(),
                  HTML('<hr style="border-color: black;">'),
                  br(),
                  h5(HTML("<b>Click below to see all the variables from the data!</b>")),
                  awesomeCheckbox(inputId = "show_non_miss",
                                  label = "Show variables without missing values.",
                                  value = FALSE),
           ),
           column(9,
                  offset = 1,
                  style = 'border-left: 1px solid',
                  column(12,
                         align = "center",
                         br(),
                         withSpinner(uiOutput("plot_mv_vis"), color = "black")
                  )
           ),
  ),
  tabPanel("Imputation",
           h3("Let's impute your missing values!"),
           h4("Select one or more imputation methods from the list below and click impute!"),
           br(),
           column(12,
                  align = "center",
                  multiInput(
                    inputId = "methods",
                    label = "Select methods:",
                    choices = NULL,
                    selected = NULL,
                    choiceNames = pull(methods_table, name),
                    choiceValues = pull(methods_table, imputomics_name),
                    width = "80%",
                    options = list(
                      non_selected_header = "Available methods:",
                      selected_header = "You have selected:"
                    )
                  ),
                  br(),
                  actionBttn(inputId = "impute_btn",
                             label = "   Impute! ",
                             style = "material-flat",
                             color = "warning",
                             size = "lg",
                             icon = icon("pen"))
           ),
           column(10, offset = 1, style = "position:absolute; bottom: 5px;",
                  progressBar(id = "progress_bar",
                              value = 0,
                              status = "success",
                              size = "xs",
                              display_pct = TRUE,
                              title = "")
           )
  ),
  tabPanel("Results",
           h2("Here you can check the results!"),
           column(3,
                  style = 'border-right: 1px solid',
                  h3("Imputation summary:"),
                  br(),
                  h4(icon("check"), "Success:"),
                  radioButtons("success_methods",
                               label = "",
                               choices = ""),
                  br(),
                  h4(icon("xmark"), "Error:"),
                  withSpinner(uiOutput("error_methods"), color = "black"),
           ),
           column(7, offset = 1, align = "center",
                  h3("Data preview:"),
                  withSpinner(DT::dataTableOutput("results"),
                              color = "black")

           )
  ),
  tabPanel("Summary",
           column(3,
                  style = 'border-right: 1px solid',
                  pickerInput(inputId = "plot_var",
                              label = "Select variable:",
                              choices = "",
                              multiple = FALSE,
                              options = list(`live-search` = TRUE)),
                  pickerInput(inputId = "plot_methods",
                              label = "Select methods:",
                              choices = "",
                              multiple = FALSE,
                              options = list(`live-search` = TRUE)),
           ),
           column(8, offset = 1,
                  withSpinner(plotOutput("points"))
           )
  ),
  tabPanel("Download",
           column(5,
                  h3("Select methods from the list below"),
                  checkboxGroupInput(inputId = "download_methods",
                                     label = "Methods:",
                                     choices = ""),
           ),
           column(5,
                  h3("... and click to download the results!"),
                  downloadBttn("download", "Download!",
                               color = "success",
                               style = "material-flat",
                               size = "lg")
           )
  )
)


server <- function(input, output, session) {
  dat <- reactiveValues()

  ##### loading data
  observeEvent(input[["users_path"]], {
    req(input[["NA_sign"]])

    file <- input[["users_path"]]
    req(file)
    path <- file[["datapath"]]
    ext <- tools::file_ext(path)
    validate(
      need(ext %in% c("xlsx", "csv", "rds"),
           paste("Please upload an xlsx, csv or rds file! You provided", ext))
    )
    raw_data <- switch(ext,
                       xlsx = read_xlsx(path),
                       csv = read.csv(path),
                       rds = readRDS(path))
    uploaded_data <- raw_data

    if(input[["NA_sign"]] == "zero") uploaded_data[raw_data == 0] <- NA

    dat[["missing_data"]] <- uploaded_data
    dat[["raw_data"]] <- raw_data
    dat[["n_cmp"]] <- ncol(raw_data)

    updateMaterialSwitch(session = session, "example_dat", value = FALSE)
  })

  observeEvent(dat[["missing_data"]], {
    if(sum(is.na(dat[["missing_data"]])) == 0)
      sendSweetAlert(session = session,
                     title = "Your data contains no missing values!",
                     text = "Make sure that right missing value denotement is selected!",
                     type = "warning")

    if(sum(is.na(dat[["missing_data"]])) > 0)
      sendSweetAlert(session = session,
                     title = "Success !",
                     text = "Your data is correct!",
                     type = "success")
  })


  observeEvent(input[["NA_sign"]], {
    req(input[["NA_sign"]])
    req(dat[["missing_data"]])
    if(input[["NA_sign"]] == "zero")
      dat[["missing_data"]][dat[["raw_data"]] == 0] <- NA
    else
      dat[["missing_data"]] <- dat[["raw_data"]]
  })

  output[["missing_data"]] <- DT::renderDataTable({
    req(dat[["missing_data"]])

    DT::datatable(dat[["missing_data"]],
                  editable = FALSE,
                  selection = list(selectable = FALSE),
                  options = list(scrollX = TRUE,
                                 pageLength = 10,
                                 searching = FALSE),
                  rownames = NULL
    )
  })

  observeEvent(input[["example_dat"]], {
    if(input[["example_dat"]] == TRUE){
      dat[["missing_data"]] <- read.csv("./test_data/example_set_na.csv")
      dat[["raw_data"]] <- dat[["missing_data"]]
      dat[["n_cmp"]] <- ncol(dat[["missing_data"]])
    }
  })

  ##### data vis

  output[["mv_pctg"]] <- renderTable({
    req(dat[["missing_data"]])

    summary_dat <- get_variables_table(dat[["missing_data"]])
    dat[["mv_summary"]] <- summary_dat[["mv_summary"]]

    summary_dat[["variables_table"]]

  }, colnames = FALSE)


  output[["mv_vis"]] <- renderPlot({
    req(dat[["mv_summary"]])
    req(dat[["missing_data"]])

    show_complete <- input[["show_non_miss"]]

    #check if there are missing values in the data
    if(sum(is.na(dat[["missing_data"]])) == 0) {
      sendSweetAlert(session = session,
                     title = "Your data contains no missing values!",
                     text = "We will plot all the variables!",
                     type = "warning")
      show_complete <- TRUE
    }

    if(input[["plot_type"]] == "Percentage") {
      tmp_dat <- dat[["mv_summary"]]
      if(!show_complete)
        tmp_dat <- filter(tmp_dat, `% Missing` > 0)
      plot_mv_segment(tmp_dat)

    } else {
      tmp_dat <- dat[["missing_data"]]
      if(!show_complete)
        tmp_dat <- dplyr::select(tmp_dat, where(function(x) any(is.na(x))))

      plot_mv_heatmap(tmp_dat)
    }
  })

  output[["plot_mv_vis"]] <- renderUI({
    plotOutput("mv_vis",
               height = max(dat[["n_cmp"]] * 22, 400),
               width = 1000)
  })

  # imputation

  observeEvent(input[["impute_btn"]], {

    if(is.null(dat[["missing_data"]])) {
      sendSweetAlert(session = session,
                     title = "No data!",
                     text = "Upload your data before imputation!!",
                     type = "error")
      req(dat[["missing_data"]])
    }

    if(is.null(input[["methods"]])) {
      sendSweetAlert(session = session,
                     title = "No methods!",
                     text = "Select at least one imputation method from the list!",
                     type = "warning")
      req(input[["methods"]])
    }

    methods <- input[["methods"]]
    progress <- 0
    progress_step <- 100/length(methods)

    results <- lapply(methods, function(ith_method) {

      ith_fun <- get(ith_method)
      title <- paste0("In progress ",
                      str_replace_all(str_remove(ith_method,"impute_"), "_", " "),
                      " ...")
      progress <<- progress + progress_step
      updateProgressBar(session = session,
                        id = "progress_bar",
                        value = progress,
                        title = title)
      imputed_dat <- imputomics:::safe_impute(ith_fun, dat[["missing_data"]])

      if(!any(is.na(imputed_dat)))
        return(imputed_dat)

    })

    updateProgressBar(session = session,
                      id = "progress_bar",
                      value = 100,
                      title = "Done!")

    errors <- methods[sapply(results, is.null)]
    success <- setdiff(methods, errors)

    if(length(success) == 0)
      sendSweetAlert(session = session,
                     title = "Error!",
                     text = "All of the chosen methods failed to impute your data!",
                     type = "error")

    if(length(errors) == 0)
      sendSweetAlert(session = session,
                     title = "Success!",
                     text = "Imputation is done! You can check and download the results!",
                     type = "success")

    if(length(errors) > 0 & length(success) > 0)
      sendSweetAlert(session = session,
                     title = "Warning!",
                     text = "Imputation is done! However, some of the chosen methods failed to impute your data!",
                     type = "warning")



    names(results) <- methods
    dat[["results"]] <- list(results = results,
                             methods = methods)
  })

  observeEvent(input[["methods"]], {
    updateProgressBar(session = session,
                      id = "progress_bar",
                      value = 0)
  })



  # results

  output[["error_methods"]] <- renderText({
    req(dat[["results"]])

    result_data <- dat[["results"]][["results"]]
    methods <- dat[["results"]][["methods"]]

    error <- methods[sapply(result_data, is.null)]
    error_txt <- "none"

    if(length(error) > 0) {
      error_txt <- methods_table %>%
        filter(imputomics_name %in% error) %>%
        pull(name) %>%
        paste0(collapse = "<br>")
    }
    error_txt
  })


  observeEvent(dat[["results"]], {
    req(dat[["results"]])

    result_data <- dat[["results"]][["results"]]
    methods <- dat[["results"]][["methods"]]
    success <- methods[!sapply(result_data, is.null)]

    success <- methods_table %>%
      filter(imputomics_name %in% success)

    dat[["results"]][["success"]] <- success
    vars <- colnames(dat[["results"]][["results"]][[1]])

    updateRadioButtons(session = session,
                       inputId = "success_methods",
                       label = "",
                       choices = pull(success, name),
                       selected = pull(success, name)[1])
    updatePickerInput(session = session,
                      inputId = "plot_var",
                      choices = vars,
                      selected = vars[1])
    updatePickerInput(session = session,
                      inputId = "plot_methods",
                      choices = pull(success, name),
                      selected = pull(success, name)[1])
    updateCheckboxGroupInput(session = session,
                             inputId = "download_methods",
                             choices = pull(success, name),
                             selected = pull(success, name))
  })


  output[["results"]] <- DT::renderDataTable({
    req(dat[["results"]])

    if(nrow(dat[["results"]][["success"]]) != 0) {
      method <- dat[["results"]][["success"]] %>%
        filter(name %in% input[["success_methods"]]) %>%
        pull(imputomics_name)

      res <- dat[["results"]][["results"]][[method]]

      DT::datatable(round(res, 4),
                    editable = FALSE,
                    selection = list(selectable = FALSE),
                    options = list(scrollX = TRUE,
                                   pageLength = 10,
                                   searching = FALSE),
                    rownames = NULL)
    }else{
      "No data to display!"
    }
  })



  output[["points"]] <- renderPlot({
    req(input[["plot_methods"]])
    req(input[["plot_var"]])
    req(dat[["missing_data"]])
    req(dat[["results"]])

    method <- dat[["results"]][["success"]] %>%
      filter(name %in% input[["plot_methods"]]) %>%
      pull(imputomics_name)

    res <- dat[["results"]][["results"]]
    res_var <- res[[method]][, input[["plot_var"]]]
    miss_var <- dat[["missing_data"]][, input[["plot_var"]]]

    plt_dat <- data.frame(var = res_var,
                          missing_var = miss_var)

    plt_dat %>%
      mutate(imputed = is.na(missing_var)) %>%
      ggplot() +
      geom_quasirandom(aes(y = var, x = imputed, col = imputed)) +
      ggtitle(paste0("Variable: ", input[["plot_var"]],
                     ", Method: ", input[["plot_methods"]])) +
      theme_minimal() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            axis.text.x = element_blank(),
            title = element_text(size = 18)) +
      xlab("")
  })




  #Summary

  output[["download"]] <- downloadHandler(
    filename = "results.xlsx",
    content = function(file) {
      req(dat[["results"]])
      req(input[["download_methods"]])

      wb_file <- createWorkbook()
      addWorksheet(wb_file, "original_data")
      writeData(wb_file, "original_data",
                dat[["missing_data"]], colNames = TRUE)

      result_data <- dat[["results"]][["results"]]

      methods <- dat[["results"]][["success"]] %>%
        filter(name %in% input[["download_methods"]]) %>%
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
  )


}

shinyApp(ui, server)

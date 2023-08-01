library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(shinyhelper)

library(ggplot2)
library(patchwork)
library(ggbeeswarm)

library(dplyr)
library(tidyr)
library(imputomics)
library(stringr)
library(readxl)
library(openxlsx)

source("app_supplementary/data_operations.R")
source("app_supplementary/plots.R")
source("app_supplementary/ui_supp.R")
source("app_supplementary/amelia_safe_impute.R")
source(system.file("readme_scripts.R", package = "imputomics"))

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
                  fileInput(
                    inputId = 'users_path',
                    label = "Upload file with missing values.",
                    multiple = FALSE,
                    accept = c(".csv", ".xlsx", ".rds")
                  ),
                  br(),
                  h4("2. Select missing value denotement:"),
                  selectInput(
                    "NA_sign",
                    "How is a missing value marked in your data?",
                    choices = c("zero", "NA"),
                    selected = "NA"
                  ),
                  br(),
                  h4("Click below to upload example data!"),
                  actionBttn(
                    inputId = "example_dat",
                    label = "Example data",
                    style = "material-flat",
                    color = "success",
                    icon = HTML("<i class='fa-solid fa-upload fa-bounce'></i>")
                  ),

           ),
           column(6,
                  align = "center",
                  offset = 1,
                  h3("Dataset preview:"),
                  withSpinner(DT::dataTableOutput("missing_data"), color = "black")
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
                  awesomeCheckbox(
                    inputId = "show_non_miss",
                    label = "Show variables without missing values.",
                    value = FALSE
                  ),
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
           column(3,
                  h4("Specify time limit per method below."),
                  helper(
                    numericInput(
                      "timeout",
                      label = "Provide a value between 1 and 300 in seconds",
                      value = 300, min = 1, max = 300
                    ),
                    type = "inline",
                    title = "How to set time limit?",
                    content = c("The <b>timeout</b> parameter allows users to set a time limit
                                for each missing value imputation method.
                                This duration determines the maximum time each
                                method has to complete the calculation.
                                If a method exceeds the specified time limit,
                                it will be marked as an error in the summary.
                                The default value is 300s (5 min)"),
                    size = "m",
                    buttonLabel = "Got it!"
                  ),
                  br(),
                  helper(
                    h4("Filter the fastest/most accurate methods."),
                    type = "inline",
                    title = "Fastest or most accurate metods",
                    content = c("<b>Fastest methods</b> refer to the top 10 missing value
                         imputation techniques that have demonstrated the shortest execution
                         time during simulations. These methods are optimized for efficiency
                         and are well-suited for handling missing data in large datasets or
                         scenarios where computation speed is crucial.",
                                "<b>Most accurate</b> are chosen based on their
                                     performance measured by the Normalized Root Mean
                                     Squared Error (NRMSE). The 10 best methods
                                     are those that have shown the lowest NRMSE values,
                                     indicating their superior ability to impute missing
                                     values accurately."),
                    size = "m",
                    buttonLabel = "Got it!"
                  ),

                  prettySwitch(
                    inputId = "fastest",
                    label = "Show the 10 fastest methods",
                    status = "danger",
                    value = FALSE,
                    slim = TRUE
                  ),
                  prettySwitch(
                    inputId = "best",
                    label = "Show the 10 most accurate methods",
                    status = "danger",
                    value = FALSE,
                    slim = TRUE
                  ),
                  br(),
                  br(),
                  br(),

                  column(12, align = "center",
                         actionBttn(inputId = "impute_btn",
                                    label = "Impute!",
                                    style = "material-flat",
                                    color = "warning",
                                    size = "lg",
                                    icon = icon("pen"))
                  ),
           ),
           column(9,
                  align = "center",
                  multiInput(
                    inputId = "methods",
                    label = "Select methods:",
                    selected = NULL,
                    choices = pull(methods_table, name),
                    width = "80%",
                    options = list(
                      non_selected_header = "Available methods:",
                      selected_header = "You have selected:"
                    )
                  )
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

  observe_helpers()

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

    try({ raw_data <- switch(ext,
                             xlsx = read_xlsx(path),
                             csv = read.csv(path),
                             rds = readRDS(path)) })

    uploaded_data <- raw_data

    #data validation
    uploaded_data <- validate_data(uploaded_data, session, input)

    dat[["missing_data"]] <- uploaded_data
    dat[["raw_data"]] <- raw_data
    dat[["n_cmp"]] <- ncol(raw_data)
  })

  observeEvent(dat[["missing_data"]], {
    req(dat[["missing_data"]])

    if(sum(is.na(dat[["missing_data"]])) == 0) {}
    sendSweetAlert(session = session,
                   title = "Your data contains no missing values!",
                   text = "Make sure that right missing value denotement is selected!",
                   type = "warning")

    if(sum(is.na(dat[["missing_data"]])) > 0)
      sendSweetAlert(session = session,
                     title = "Success !",
                     text = "Your data is correct!",
                     type = "success")

    dat[["mv_summary"]]  <- get_variables_table(dat[["missing_data"]])
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
    dat[["missing_data"]] <- read.csv("./test_data/im_normal.csv")
    dat[["raw_data"]] <- dat[["missing_data"]]
    dat[["n_cmp"]] <- ncol(dat[["missing_data"]])
  }, ignoreInit = TRUE)

  ##### data vis

  output[["mv_pctg"]] <- renderTable({
    req(dat[["missing_data"]])

    dat[["mv_summary"]][["variables_table"]]

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
      tmp_dat <- dat[["mv_summary"]][["mv_summary"]]
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

  ## filter methods

  observeEvent(input[["best"]], {

    if(input[["best"]]) {
      updatePrettySwitch(session = session,
                         inputId = "fastest",
                         value = FALSE)
      updateMultiInput(session = session,
                       inputId = "methods",
                       choices = pull(filter(methods_table, best), name),
                       selected = input[["methods"]])
    }else {
      if(!input[["fastest"]])
        updateMultiInput(session = session,
                         inputId = "methods",
                         choices = pull(methods_table, name),
                         selected = input[["methods"]])
    }
  }, ignoreInit = TRUE)

  observeEvent(input[["fastest"]], {
    if(input[["fastest"]]){
      updatePrettySwitch(session = session,
                         inputId = "best",
                         value = FALSE)
      updateMultiInput(session = session,
                       inputId = "methods",
                       choices = pull(filter(methods_table, fastest), name),
                       selected = input[["methods"]])
    }else {
      if(!input[["best"]])
        updateMultiInput(session = session,
                         inputId = "methods",
                         choices = pull(methods_table, name),
                         selected = input[["methods"]])
    }
  }, ignoreInit = TRUE)


  ## imputation calc

  observeEvent(input[["impute_btn"]], {
    req(input[["timeout"]])

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

    methods <- methods_table %>%
      filter(name %in% input[["methods"]]) %>%
      pull(imputomics_name)
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
      if(ith_method == "impute_amelia") {
        imputed_dat <- safe_impute_amelia(ith_fun,
                                          dat[["missing_data"]],
                                          timeout = input[["timeout"]])
      } else {
        imputed_dat <- imputomics:::safe_impute(ith_fun,
                                                dat[["missing_data"]],
                                                timeout = input[["timeout"]])
      }

      if(!any(is.na(imputed_dat)) & !inherits(imputed_dat, "try-error"))
        return(imputed_dat)
      else
        return(NULL)
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
                     text = "All of the chosen methods failed to impute your data in provided time!
                     Try to increase the time limit or validate your dataset.",
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

    complete_cols <- dat[["mv_summary"]][["mv_summary"]] %>%
      filter(`% Missing` == 0) %>%
      pull(Variable)

    vars <- setdiff(colnames(dat[["results"]][["results"]][[1]]),
                    complete_cols)

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
      res <- data.frame("No data to display!", row.names = NULL)
      colnames(res) <- NULL
      res
    }
  })



  output[["points"]] <- renderPlot({
    req(input[["plot_methods"]])
    req(input[["plot_var"]])
    req(dat[["missing_data"]])
    req(dat[["results"]])

    plot_points_density(dat, input)

  })



  #Summary

  output[["download"]] <- downloadHandler(
    filename = "results.xlsx",
    content = function(file) {
      req(dat[["results"]])
      req(input[["download_methods"]])

      save_excel(dat, file, download_methods = input[["download_methods"]])
    }
  )


}

shinyApp(ui, server)

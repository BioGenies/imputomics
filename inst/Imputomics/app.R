library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(shinyhelper)
library(colourpicker)

library(ggplot2)
library(patchwork)
library(ggbeeswarm)
library(ggvenn)

library(dplyr)
library(tidyr)
library(imputomics)
library(stringr)
library(readxl)
library(openxlsx)

source("app_supplementary/data_operations.R")
source("app_supplementary/download_plot_module.R")
source("app_supplementary/plots.R")
source("app_supplementary/ui_supp.R")
source("app_supplementary/amelia_safe_impute.R")
source("app_supplementary/pattern_switch_module.R")
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
           column(3,
                  style = 'border-right: 1px solid',
                  h3("Here you can upload your data!", style = "font-size:20px;"),
                  br(),
                  h5("1. Select your metabolomics dataset in CSV, Excel or RDS:",
                     style = "font-size:15px;"),
                  fileInput(
                    inputId = 'users_path',
                    label = "Upload file with missing values.",
                    multiple = FALSE,
                    accept = c(".csv", ".xlsx", ".rds")
                  ),
                  h5("2. Select missing value denotement:",
                     style = "font-size:15px;"),
                  selectInput(
                    "NA_sign",
                    "How is a missing value marked in your data?",
                    choices = c("0", "1", "NA"),
                    selected = "NA"
                  ),
                  br(),
                  h5("Click below to upload example data!",
                     style = "font-size:15px;"),
                  actionBttn(
                    inputId = "example_dat",
                    label = "Example data",
                    style = "material-flat",
                    color = "success",
                    icon = HTML("<i class='fa-solid fa-upload fa-bounce'></i>")
                  ),
           ),
           column(8,
                  tabsetPanel(
                    tabPanel(
                      "Entire dataset",
                      br(),
                      br(),
                      column(
                        10, offset = 1,
                        withSpinner(DT::dataTableOutput("full_data"), color = "black")
                      )
                    ),
                    tabPanel(
                      "Manage Variables",
                      h3("Exclude variables from imputation data."),
                      h4("Choose from the numeric columns which ones to exclude.
                         Categorical/text columns will be automatically ignored."),
                      column(6, offset = 1,
                             multiInput(
                               inputId = "columns",
                               label = "",
                               selected = NULL,
                               choices = character(0),
                               width = "90%",
                               options = list(
                                 non_selected_header = "Numeric columns:",
                                 selected_header = "Categorical columns:"
                               )
                             )
                      ),
                      column(4,
                             br(),
                             br(),
                             h4("Columns to be ignored during imputation:"),
                             htmlOutput("nonnumeric_cols_ui")
                      )
                    ),
                    tabPanel(
                      "Imputation dataset",
                      br(),
                      br(),
                      column(
                        10, offset = 1,
                        withSpinner(DT::dataTableOutput("missing_num_data"), color = "black")
                      )
                    ),
                  )
           ),
  ),
  navbarMenu("Missing values analysis",
             tabPanel("Visualization",
                      tabsetPanel(
                        tabPanel(
                          "Missing values percentage",
                          br(),
                          column(3,
                                 style = 'border-right: 1px solid',
                                 h3("Plot settings:"),
                                 br(),
                                 sliderInput("thresh",
                                             "Select threshold for misisng values ratio [%].",
                                             value = 20, min = 0, max = 100),
                                 br(),
                                 colourInput("above_threshold_col",
                                             "Above threshold color",
                                             "tomato"),
                                 colourInput("below_threshold_col",
                                             "Below threshold color",
                                             "black"),
                                 br(),
                                 awesomeCheckbox(
                                   inputId = "show_non_miss",
                                   label = "Show variables without missing values.",
                                   value = FALSE
                                 )
                          ),
                          column(7,
                                 offset = 1,
                                 withSpinner(uiOutput("plot_segment_ui"),
                                             color = "black"),
                          ),
                          download_plot_UI("segment"),
                        ),
                        tabPanel(
                          "Missing values pattern heatmap",
                          br(),
                          column(3,
                                 style = 'border-right: 1px solid',
                                 h3("Plot settings:"),
                                 br(),
                                 colourInput("missing_col",
                                             "Missing color",
                                             "black"),
                                 colourInput("nonmissing_col",
                                             "Non-missing color",
                                             "gray"),
                                 br(),
                                 awesomeCheckbox(
                                   inputId = "show_non_miss",
                                   label = "Show variables without missing values.",
                                   value = FALSE
                                 )
                          ),
                          column(7,
                                 offset = 1,
                                 withSpinner(uiOutput("plot_heatmap_ui"),
                                             color = "black")
                          ),
                          download_plot_UI("heatmap")
                        )
                      ),
             ),
             tabPanel("Data management",
                      h3("Variables removal"),
                      column(3,
                             h4("1. Set threshold for missing values ratio",
                                style = "font-size:15px;"),
                             sliderInput("remove_threshold",
                                         label = "Select maximum ratio allowed for each variable.",
                                         min = 0,
                                         max = 100,
                                         value = 20,
                                         step = 1,
                                         width = '100%'),
                             br(),
                             helper(
                               h4("2. Set groups (optional)",
                                  style = "font-size:15px;"),
                               type = "inline",
                               title = "Variables removal using groups.",
                               content = "Only variables exceeding the specified missing
                               value ratio threshold within each group will be removed.
                               When choosing a group for variable removal, please note
                               that only character variables without missing values
                               will be available for selection.",
                               size = "m",
                               buttonLabel = "Got it!"
                             ),
                             selectInput("group",
                                         label = "Select grouping variable",
                                         choices = NULL,
                                         selected = NULL,
                                         multiple = FALSE),
                             br(),
                             h4("3. Click Remove!",
                                style = "font-size:15px;"),
                             fluidRow(
                               column(3,
                                      align = "center",
                                      offset = 1,
                                      actionButton("remove_btn", label = "Remove", icon = icon("trash"))),
                               column(3,
                                      offset = 1,
                                      align = "center",
                                      actionButton("undo_btn",label = "Undo", icon = icon("rotate-left")))
                             ),
                             HTML('<hr style="border-color: black;">'),
                             br(),
                             h5("The following variables will be removed:"),
                             htmlOutput("to_remove_names"),
                      ),
                      column(4,
                             style = 'border-right: 1px solid;border-left: 1px solid',
                             h4("Ratio of missing data per group [%]"),
                             br(),
                             withSpinner(DT::dataTableOutput("mv_ratio"), color = "black"),
                      ),

                      column(5,
                             h4("Venna diagram (from 2 to 4 groups):"),
                             br(),
                             withSpinner(plotOutput("venna_diagram",
                                                    width = '100%',
                                                    height = 500),
                                         color = "black")
                      )
             ),
  ),
  tabPanel("Imputation",
           column(5,
                  h3("Let's impute your missing values!", style = "font-size:22px;"),
                  h5("Select one or more imputation methods from the list below and click impute!",
                     style = "font-size:15px;"),
                  h5("Check the references panel for the references of all imputations methods."),
                  br(),
                  helper(
                    h4("Specify time limit per method below.", style = "font-size:15px;"),
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
                  numericInput(
                    "timeout",
                    label = "Provide a value between 1 and 300 in seconds",
                    value = 300, min = 1, max = 300
                  ),
                  br(),
                  helper(
                    add_methods_UI("fastest"),
                    type = "inline",
                    title = "Fastest or most accurate metods",
                    content = c(
                      "<b>Fastest methods</b> refer to the top 10
                    missing value imputation techniques that have demonstrated
                    the shortest execution time during simulations. These methods
                    are optimized for efficiency and are well-suited for handling
                    missing data in large datasets or scenarios where
                      computation speed is crucial.",
                      "<b>Most accurate (generally/for MCAR/for MAR/for MNAR)</b>
                      are chosen based on their
                      performance measured by the Normalized Root Mean Squared
                      Error (NRMSE). The best methods are those that have
                      frequency of successful imputation over 80% of the cases
                      and have shown the lowest NRMSE values, indicating their
                      superior ability to impute missing values accurately.",
                      "<b>MCAR (Missing Completely At Random): </b> missing values
                      are due to random errors and stochastic fluctuations during
                      the data acquisition process, such as incomplete derivation
                      or ionization.",
                      "<b>MAR (Missing At Random):</b> assumes that other observed
                      variables determine the possibility of missingness.",
                      "<b>MNAR (Missing Not At Random):</b> is considered for
                      censored missing values, that are caused by the limit of
                      detection (LOD) of a device </br> ",
                      "For more details see:  Jarosław Chilimoniuk, Krystyna Grzesiak, Jakub Kała,
                      Dominik Nowakowski, Adam Krętowski, Rafał Kolenda, Michał
                      Ciborowski, Michał Burdukiewicz (2023). imputomics: web
                      server and R package for missing values imputation in
                      metabolomics data"
                    ),
                    size = "m",
                    buttonLabel = "Got it!"
                  ),
                  add_methods_UI("best"),
                  add_methods_UI("MCAR"),
                  add_methods_UI("MAR"),
                  add_methods_UI("MNAR"),
                  textOutput("n_methods"),
                  br(),
                  column(12, align = "center",
                         actionBttn(inputId = "impute_btn",
                                    label = "Impute!",
                                    style = "material-flat",
                                    color = "warning",
                                    size = "lg",
                                    icon = icon("pen")),
                  ),
                  h4("Usage note:", style = "font-size:15px;"),
                  h5(HTML("Searching for methods based on fitting a predefined hypothesis is
           associated with the issue of <b>multiple comparisons</b> and may lead to
           significant <b>overfitting</b>. When choosing an imputation method, prioritize the structure of
              your data over preconceived notions. Consider
              nuances such as distribution, scale, missing value patterns, and
              relationships for a more accurate and reliable outcome, adhering
              to best practices in data analysis."), style = "font-size:12px;"),
           ),
           column(7,
                  align = "center",
                  br(),
                  br(),
                  br(),
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
                  ),
                  "*The most resource demanding tools (MAI and Gibbs Sampler
                     based methods) are available only in the R package.",
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
           h3("Here you can check the results!"),
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
           fluidRow(
             column(3,
                    style = 'border-right: 1px solid',
                    h3("Plot settings:"),
                    br(),
                    pickerInput(inputId = "plot_var",
                                label = "Select variable:",
                                choices = "",
                                multiple = FALSE,
                                options = list(`live-search` = TRUE)),
                    pickerInput(inputId = "plot_methods",
                                label = "Select method:",
                                choices = "",
                                multiple = FALSE,
                                options = list(`live-search` = TRUE)),
                    br(),
                    colourInput("missing_col_res",
                                "Imputed data color:",
                                "tomato"),
                    colourInput("nonmissing_col_res",
                                "Observed data color:",
                                "black"),
                    br(),
             ),
             column(7, offset = 1, br(),
                    withSpinner(plotOutput("points", height = 500))),
             column(1, download_plot_UI("points"))
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
  ),
  tabPanel("References",
           column(8, offset = 1,
                  includeMarkdown("citations.md"),
           ),
  )
)

################################################################################


server <- function(input, output, session) {
  dat <- reactiveValues()

  observe_helpers()

  ##### loading data
  observeEvent(input[["users_path"]], {
    req(input[["NA_sign"]])

    uploaded_data <- NULL
    raw_data <- NULL

    file <- input[["users_path"]]
    req(file)
    path <- file[["datapath"]]
    ext <- tools::file_ext(path)
    validate(
      need(ext %in% c("xlsx", "csv", "rds"),
           paste("Please upload an xlsx, csv or rds file! You provided", ext))
    )
    try({ uploaded_data <- switch(ext,
                                  xlsx = read_xlsx(path),
                                  csv = read.csv(path),
                                  rds = readRDS(path)) })

    dat[["raw_data"]] <- uploaded_data
    dat[["uploaded_data"]] <- uploaded_data
    dat[["missing_data"]] <- validate_data(uploaded_data, session, input)

    if(!is.null(dat[["missing_data"]])) {
      dat[["n_cmp"]] <- ncol(uploaded_data)
      dat[["all_cols"]] <- colnames(dat[["uploaded_data"]])
      dat[["nonnumeric_cols_raw"]] <- colnames(select_if(dat[["uploaded_data"]], ~ !is.numeric(.)))
      dat[["nonnumeric_cols"]] <- dat[["nonnumeric_cols_raw"]]

      if(!(length(dat[["nonnumeric_cols"]]) == 0)) {
        grouping_cols <- dat[["uploaded_data"]] %>%
          dplyr::select(dat[["nonnumeric_cols"]]) %>%
          select_if(~ !any(is.na(.))) %>%
          colnames()
      } else {
        grouping_cols <- character(0)
      }

      updateMultiInput(session,
                       inputId = "columns",
                       choices = colnames(dat[["missing_data"]]))
      updateSelectInput(session,
                        inputId = "group",
                        choices = c("none", grouping_cols),
                        selected = "none")
    } else {
      dat[["uploaded_data"]] <- NULL
      dat[["raw_data"]] <- NULL
    }
  })


  observeEvent(dat[["missing_data"]], {
    req(dat[["missing_data"]])

    if(sum(is.na(dat[["missing_data"]])) == 0)
      sendSweetAlert(session = session,
                     title = "Your data contains no missing values!",
                     text = "Make sure that right missing value denotement is selected!",
                     type = "warning")

    # if(sum(is.na(dat[["missing_data"]])) > 0)
    #   sendSweetAlert(session = session,
    #                  title = "Success !",
    #                  text = "Your data is correct!",
    #                  type = "success")
    dat[["mv_summary"]]  <- get_variables_table(dat[["missing_data"]])
  })


  output[["nonnumeric_cols_ui"]] <- renderUI({
    HTML(ifelse(length(dat[["nonnumeric_cols"]]) > 0,
                paste0(paste(dat[["nonnumeric_cols"]], collapse = ", <br/>"),
                       "<br/><br/> Total: ",
                       length(dat[["nonnumeric_cols"]]),
                       " variables."),
                "none"))
  })


  observeEvent(input[["columns"]], {
    req(dat[["uploaded_data"]])
    req(dat[["uploaded_data"]])

    dat[["nonnumeric_cols"]] <- union(dat[["nonnumeric_cols_raw"]], input[["columns"]])
    dat[["missing_data"]] <- dat[["uploaded_data"]] %>%
      dplyr::select(-dat[["nonnumeric_cols"]])
    if(ncol(dat[["missing_data"]]) == 0) {
      sendSweetAlert(session = session,
                     title = "No numeric columns!",
                     text = "Make sure that the uploaded file contains dataset with numeric columns.",
                     type = "error")
      dat[["missing_data"]] <- NULL
    }
  }, ignoreNULL = FALSE)


  observeEvent(input[["NA_sign"]], {
    req(input[["NA_sign"]])
    req(dat[["uploaded_data"]])
    req(dat[["raw_data"]])
    req(dat[["missing_data"]])

    tmp_missing_data <- dat[["raw_data"]][, colnames(dat[["missing_data"]])]

    if(input[["NA_sign"]] == "0") {
      dat[["missing_data"]][tmp_missing_data == 0] <- NA
      dat[["uploaded_data"]][dat[["raw_data"]] == 0] <- NA
    }
    if(input[["NA_sign"]] == "1") {
      dat[["missing_data"]][tmp_missing_data == 1] <- NA
      dat[["uploaded_data"]][dat[["raw_data"]] == 1] <- NA
    }
    if(input[["NA_sign"]] == "NA") {
      dat[["missing_data"]][is.na(tmp_missing_data)] <- NA
      dat[["uploaded_data"]][is.na(dat[["raw_data"]])] <- NA
    }
  })


  output[["full_data"]] <- DT::renderDataTable({
    req(dat[["uploaded_data"]])
    DT::datatable(round_numeric(dat[["uploaded_data"]]),
                  editable = FALSE,
                  selection = list(selectable = FALSE),
                  options = list(scrollX = TRUE,
                                 pageLength = 10,
                                 searching = FALSE),
                  rownames = NULL)
  })


  output[["missing_num_data"]] <- DT::renderDataTable({
    req(dat[["missing_data"]])
    DT::datatable(round_numeric(dat[["missing_data"]]),
                  editable = FALSE,
                  selection = list(selectable = FALSE),
                  options = list(scrollX = TRUE,
                                 pageLength = 10,
                                 searching = FALSE),
                  rownames = NULL)
  })


  output[["missing_data"]] <- DT::renderDataTable({
    req(dat[["uploaded_data"]])

    DT::datatable(round_numeric(dat[["uploaded_data"]]),
                  editable = FALSE,
                  selection = list(selectable = FALSE),
                  options = list(scrollX = TRUE,
                                 pageLength = 10,
                                 searching = FALSE),
                  rownames = NULL
    )
  })


  observeEvent(input[["example_dat"]], {
    dat[["uploaded_data"]] <- read.csv("./test_data/example_dat.csv")
    dat[["missing_data"]] <-  dat[["uploaded_data"]][, sapply(dat[["uploaded_data"]], is.numeric)]
    dat[["n_cmp"]] <- ncol(dat[["missing_data"]])

    dat[["all_cols"]] <- colnames(dat[["uploaded_data"]])
    dat[["nonnumeric_cols_raw"]] <- colnames(select_if(dat[["uploaded_data"]], ~ !is.numeric(.)))
    dat[["nonnumeric_cols"]] <- dat[["nonnumeric_cols_raw"]]

    grouping_cols <- dat[["uploaded_data"]] %>%
      dplyr::select(dat[["nonnumeric_cols"]]) %>%
      select_if(~ !any(is.na(.))) %>%
      colnames()

    updateMultiInput(session,
                     inputId = "columns",
                     choices = colnames(dat[["missing_data"]]))
    updateSelectInput(session,
                      inputId = "group",
                      choices = c("none", grouping_cols),
                      selected = "none")

  }, ignoreInit = TRUE)


  ##### data vis


  plot_segment <- reactive({
    req(dat[["mv_summary"]])
    req(input[["thresh"]])
    req(input[["below_threshold_col"]])
    req(input[["above_threshold_col"]])
    show_complete <- input[["show_non_miss"]]

    #check if there are missing values in the data
    if(sum(is.na(dat[["uploaded_data"]])) == 0) {
      sendSweetAlert(session = session,
                     title = "Your data contains no missing values!",
                     text = "We will plot all the variables!",
                     type = "warning")
      show_complete <- TRUE
    }
    tmp_dat <- dat[["mv_summary"]][["mv_summary"]]
    if(!show_complete)
      tmp_dat <- filter(tmp_dat, `% Missing` > 0)
    plot_mv_segment(tmp_dat,
                    thresh = input[["thresh"]],
                    below_col = input[["below_threshold_col"]],
                    above_col = input[["above_threshold_col"]])
  })


  output[["plot_segment"]] <- renderPlot({
    plot_segment()
  })


  download_plot_SERVER("segment", plot_reactive = plot_segment)


  output[["plot_segment_ui"]] <- renderUI({
    req(dat[["mv_summary"]])
    req(dat[["missing_data"]])
    plotOutput("plot_segment",
               height = max(dat[["n_cmp"]] * 22, 400),
               width = 800)
  })


  plot_heatmap <- reactive({
    req(dat[["uploaded_data"]])
    req(dat[["uploaded_data"]])
    show_complete <- input[["show_non_miss"]]

    #check if there are missing values in the data
    if(sum(is.na(dat[["uploaded_data"]])) == 0) {
      sendSweetAlert(session = session,
                     title = "Your data contains no missing values!",
                     text = "We will plot all the variables!",
                     type = "warning")
      show_complete <- TRUE
    }

    tmp_dat <- dat[["uploaded_data"]]
    if(!show_complete)
      tmp_dat <- dplyr::select(tmp_dat, where(function(x) any(is.na(x))))

    plot_mv_heatmap(tmp_dat,
                    missing_col = input[["missing_col"]],
                    nonmissing_col = input[["nonmissing_col"]])
  })


  output[["plot_heatmap"]] <- renderPlot({
    plot_heatmap()
  })


  download_plot_SERVER("heatmap", plot_reactive = plot_heatmap)


  output[["plot_heatmap_ui"]] <- renderUI({
    plotOutput("plot_heatmap",
               height = max(dat[["n_cmp"]] * 22, 400),
               width = 800)
  })


  ratio_table <- reactive({
    req(dat[["missing_data"]])
    req(dat[["uploaded_data"]])
    req(dat[["mv_summary"]])
    numeric_vars <- colnames(dat[["uploaded_data"]][, sapply(dat[["uploaded_data"]], is.numeric)])

    if(input[["group"]] == "none")
      ratio_table <- dat[["mv_summary"]][["mv_summary"]]
    else
      ratio_table <- dat[["missing_data"]] %>%
      mutate(group = pull(dat[["uploaded_data"]], input[["group"]])) %>%
      gather(Variable, measurement, -group) %>%
      group_by(group, Variable) %>%
      summarise(missing_ratio = mean(is.na(measurement)) * 100) %>%
      spread(group, missing_ratio)

    ratio_table
  })


  output[["mv_ratio"]] <- DT::renderDataTable({
    ratios <- ratio_table()
    colnames(ratios)[1] <- "Variable"
    DT::datatable(round_numeric(ratios),
                  editable = FALSE,
                  selection = list(selectable = FALSE),
                  options = list(scrollX = TRUE,
                                 paging = FALSE,
                                 scrollY = 400,
                                 searching = FALSE),
                  rownames = NULL
    )
  })


  to_remove <- reactive({
    req(input[["remove_threshold"]])
    ratio_table() %>%
      mutate(to_remove = rowSums(across(where(is.numeric)) >= input[["remove_threshold"]]) == (ncol(.) - 1)) %>%
      filter(to_remove) %>%
      pull(Variable)
  })


  output[["to_remove_names"]] <- renderUI({
    get_remove_html_content(to_remove())
  })


  observeEvent(input[["remove_btn"]], {
    req(dat[["missing_data"]])
    dat[["removed"]] <- to_remove()
    dat[["missing_data"]] <- dat[["missing_data"]] %>%
      dplyr::select(-to_remove())
  })


  observeEvent(input[["undo_btn"]], {
    req(dat[["uploaded_data"]])
    dat[["missing_data"]] <- dat[["uploaded_data"]][, sapply(dat[["uploaded_data"]], is.numeric)]
    dat[["removed"]] <- character(0)
  })


  venna_diagram <- reactive({
    req(input[["remove_threshold"]])
    req(input[["group"]])
    req(dat[["uploaded_data"]])

    if(input[["group"]] != "none") {
      groups <- unique(dat[["uploaded_data"]][, input[["group"]]])

      ratios <- ratio_table() %>%
        mutate(across(groups, greater_eq_than_thresh, thresh = input[["remove_threshold"]]))

      grouped_variables <- lapply(groups, function(ith_group) {
        ratios %>%
          filter(get(ith_group)) %>%
          pull(Variable)
      })
      names(grouped_variables) <- groups

      if(!(length(groups) > 4 )) {
        return(ggvenn(grouped_variables))
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  })


  output[["venna_diagram"]] <- renderPlot({
    venna_diagram()
  })


  # imputation

  ## filter methods

  output[["n_methods"]] <- renderText({
    n <- length(input[["methods"]])

    paste0("Number of chosen methods: ", n)
  })

  add_methods_SERVER("fastest", input, session, methods_table)
  add_methods_SERVER("best", input, session, methods_table)
  add_methods_SERVER("MCAR", input, session, methods_table)
  add_methods_SERVER("MAR", input, session, methods_table)
  add_methods_SERVER("MNAR", input, session, methods_table)


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

      if(ith_method == "impute_amelia")
        imputed_dat <- safe_impute_amelia(ith_fun,
                                          dat[["missing_data"]],
                                          timeout = input[["timeout"]])
      else
        imputed_dat <- imputomics:::safe_impute(ith_fun,
                                                dat[["missing_data"]],
                                                timeout = input[["timeout"]])
      if(!any(is.na(imputed_dat)) & !inherits(imputed_dat, "try-error"))
        return(imputed_dat)
      else
        return(NULL)
    })

    updateProgressBar(session = session, id = "progress_bar", value = 100, title = "Done!")
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


  observeEvent(input[["methods"]], {updateProgressBar(session = session,
                                                      id = "progress_bar",
                                                      value = 0) })


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
    success <- methods_table %>%
      filter(imputomics_name %in% methods[!sapply(result_data, is.null)])

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
    } else{
      res <- data.frame("No data to display!", row.names = NULL)
      colnames(res) <- NULL
      res
    }
  })


  plot_points <- reactive({
    req(input[["plot_methods"]])
    req(input[["plot_var"]])
    req(dat[["missing_data"]])
    req(dat[["results"]])
    plot_points_density(dat, input)
  })


  output[["points"]] <- renderPlot({
    plot_points()
  })


  point_plot_dat <- reactive({
    req(input[["plot_var"]])
    req(input[["plot_methods"]])
    paste0(input[["plot_var"]], "_", input[["plot_methods"]])
  })


  download_plot_SERVER("points",
                       plot_reactive = plot_points,
                       point_plot_dat = point_plot_dat)


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

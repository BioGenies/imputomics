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
           column(3,
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
                    choices = c("0", "1", "NA"),
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
           column(7,
                  offset = 1,
                  h3("Dataset preview:"),
                  br(),
                  withSpinner(DT::dataTableOutput("missing_num_data"), color = "black")
           )
  ),
  navbarMenu("Missing values analysis",
             tabPanel("Data menagement",
                      column(3,
                             style = 'border-right: 1px solid',
                             helper(
                               h4("Non-informative variables removal"),
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
                             h4("1. Set threshold for missing values ratio"),
                             sliderInput("remove_threshold",
                                         label = "Select maximum ratio allowed for each variable.",
                                         min = 0,
                                         max = 100,
                                         value = 20,
                                         step = 1,
                                         width = '100%'),
                             h4("2. Set groups (optional)"),
                             selectInput("group",
                                         label = "Select grouping variable",
                                         choices = NULL,
                                         selected = NULL,
                                         multiple = FALSE),
                             h4("3. Click Remove!"),
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
                             h4("The following variables will be removed:"),
                             htmlOutput("to_remove_names"),
                      ),
                      column(4,
                             h4("Ratio of missing data per group [%]"),
                             br(),
                             withSpinner(DT::dataTableOutput("mv_ratio"), color = "black"),
                      ),

                      column(5,
                             br(),
                             withSpinner(plotOutput("venna_diagram",
                                                    width = '100%',
                                                    height = 500),
                                         color = "black")
                      )
             ),
             tabPanel("Visualization",
                      tabsetPanel(
                        tabPanel("Missing values percentage",
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
                                 column(8,
                                        offset = 1,
                                        withSpinner(uiOutput("plot_segment_ui"),
                                                    color = "black")
                                 ),
                        ),
                        tabPanel("Missing values pattern heatmap",
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
                                 column(8,
                                        offset = 1,
                                        withSpinner(uiOutput("plot_heatmap_ui"),
                                                    color = "black")
                                 )
                        )
                      ),
             )
  ),
  tabPanel("Imputation",
           h3("Let's impute your missing values!"),
           h4("Select one or more imputation methods from the list below and click impute!"),
           h5("Check the references panel for the references of all imputations methods."),
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
                    content = c(
                      "<b>Fastest methods</b> refer to the top 10
                    missing value imputation techniques that have demonstrated
                    the shortest execution time during simulations. These methods
                    are optimized for efficiency and are well-suited for handling
                    missing data in large datasets or scenarios where
                      computation speed is crucial.",
                      "<b>Most accurate</b> are chosen based on their
                      performance measured by the Normalized Root Mean Squared
                      Error (NRMSE). The 10 best methods are those that have
                      frequency of successful imputation over 80% of the cases
                      and have shown the lowest NRMSE values, indicating their
                      superior ability to impute missing values accurately."
                    ),
                    size = "m",
                    buttonLabel = "Got it!"
                  ),

                  prettySwitch(
                    inputId = "fastest",
                    label = "Add the 10 fastest methods",
                    status = "danger",
                    value = FALSE,
                    slim = TRUE
                  ),
                  prettySwitch(
                    inputId = "best",
                    label = "Add the 10 most accurate methods",
                    status = "danger",
                    value = FALSE,
                    slim = TRUE
                  ),
                  br(),
                  textOutput("n_methods"),
                  br(),
                  "*The most resource demanding tools (MAI and Gibbs Sampler
                     based methods) are available only in the R package.",
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
                  br(),
                  h5("Dear User,"),
                  h5(" Selecting imputation methods based solely on preconceived notions can compromise data integrity. Resist the urge to cherry-pick. Instead, explore a variety of techniques to ensure robust handling of missing data."),
                  h5(HTML("Best,<br>imputomics team"))
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

    raw_data <- NULL
    uploaded_data <- NULL

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
    checked_data <- validate_data(uploaded_data, session, input)

    dat[["missing_data"]] <- checked_data[["uploaded_data"]]
    dat[["full_data"]] <- checked_data[["full_data"]]
    dat[["raw_data"]] <- raw_data
    dat[["n_cmp"]] <- ncol(raw_data)

    updateSelectInput(session,
                      inputId = "group",
                      choices = c("none",
                                  setdiff(colnames(dat[["full_data"]]),
                                          colnames(dat[["missing_data"]]))),
                      selected = "none")
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


  observeEvent(input[["NA_sign"]], {
    req(input[["NA_sign"]])
    req(dat[["missing_data"]])

    if(input[["NA_sign"]] == "0")
      dat[["missing_data"]][dat[["raw_data"]] == 0] <- NA

    if(input[["NA_sign"]] == "1")
      dat[["missing_data"]][dat[["raw_data"]] == 1] <- NA

    if(input[["NA_sign"]] == "NA")
      dat[["missing_data"]] <- dat[["raw_data"]]

  })

  output[["missing_num_data"]] <- DT::renderDataTable({
    req(dat[["missing_data"]])

    DT::datatable(round_numeric(dat[["missing_data"]]),
                  editable = FALSE,
                  selection = list(selectable = FALSE),
                  options = list(scrollX = TRUE,
                                 pageLength = 10,
                                 searching = FALSE),
                  rownames = NULL
    )
  })


  output[["missing_data"]] <- DT::renderDataTable({
    req(dat[["full_data"]])

    DT::datatable(round_numeric(dat[["full_data"]]),
                  editable = FALSE,
                  selection = list(selectable = FALSE),
                  options = list(scrollX = TRUE,
                                 pageLength = 10,
                                 searching = FALSE),
                  rownames = NULL
    )
  })


  observeEvent(input[["example_dat"]], {
    dat[["full_data"]] <- read.csv("./test_data/example_dat.csv")
    dat[["missing_data"]] <-  dat[["full_data"]][, sapply(dat[["full_data"]], is.numeric)]
    dat[["raw_data"]] <- dat[["missing_data"]]
    dat[["n_cmp"]] <- ncol(dat[["missing_data"]])

    updateSelectInput(session,
                      inputId = "group",
                      choices = c("none",
                                  setdiff(colnames(dat[["full_data"]]),
                                          colnames(dat[["missing_data"]]))),
                      selected = "none")

  }, ignoreInit = TRUE)


  ##### data vis

  output[["plot_segment"]] <- renderPlot({
    req(dat[["mv_summary"]])
    req(input[["thresh"]])
    req(input[["below_threshold_col"]])
    req(input[["above_threshold_col"]])
    show_complete <- input[["show_non_miss"]]

    #check if there are missing values in the data
    if(sum(is.na(dat[["full_data"]])) == 0) {
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


  output[["plot_segment_ui"]] <- renderUI({
    req(dat[["mv_summary"]])
    req(dat[["missing_data"]])
    plotOutput("plot_segment",
               height = max(dat[["n_cmp"]] * 22, 400),
               width = 800)
  })


  output[["plot_heatmap"]] <- renderPlot({
    req(dat[["full_data"]])
    show_complete <- input[["show_non_miss"]]

    #check if there are missing values in the data
    if(sum(is.na(dat[["full_data"]])) == 0) {
      sendSweetAlert(session = session,
                     title = "Your data contains no missing values!",
                     text = "We will plot all the variables!",
                     type = "warning")
      show_complete <- TRUE
    }

    tmp_dat <- dat[["full_data"]]
    if(!show_complete)
      tmp_dat <- dplyr::select(tmp_dat, where(function(x) any(is.na(x))))

    plot_mv_heatmap(tmp_dat,
                    missing_col = input[["missing_col"]],
                    nonmissing_col = input[["nonmissing_col"]])
  })


  output[["plot_heatmap_ui"]] <- renderUI({
    plotOutput("plot_heatmap",
               height = max(dat[["n_cmp"]] * 22, 400),
               width = 800)
  })

  ratio_table <- reactive({
    req(dat[["missing_data"]])
    req(dat[["full_data"]])
    req(dat[["mv_summary"]])

    numeric_vars <- colnames(dat[["full_data"]][, sapply(dat[["full_data"]], is.numeric)])

    if(input[["group"]] == "none")
      ratio_table <- dat[["mv_summary"]][["mv_summary"]]
    else
      ratio_table <- dat[["missing_data"]] %>%
      mutate(group = pull(dat[["full_data"]], input[["group"]])) %>%
      gather(variable, measurement, -group) %>%
      group_by(group, variable) %>%
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
                                 scrollY = 500,
                                 searching = FALSE),
                  rownames = NULL
    )
  })


  to_remove <- reactive({
    req(input[["remove_threshold"]])

    ratio_table() %>%
      mutate(to_remove = rowSums(across(where(is.numeric)) >= input[["remove_threshold"]]) == (ncol(.) - 1)) %>%
      filter(to_remove) %>%
      pull(variable)
  })


  output[["to_remove_names"]] <- renderUI({
    get_remove_html_content(to_remove())
  })


  observeEvent(input[["remove_btn"]], {
    req(dat[["missing_data"]])

    dat[["missing_data"]] <- dat[["missing_data"]] %>%
      select(-to_remove())

  })


  observeEvent(input[["undo_btn"]], {
    req(dat[["full_data"]])
    dat[["missing_data"]] <- dat[["full_data"]][, sapply(dat[["full_data"]], is.numeric)]
  })


  output[["venna_diagram"]] <- renderPlot({
    req(input[["remove_threshold"]])
    req(input[["group"]])
    req(dat[["full_data"]])

    if(input[["group"]] != "none") {
      groups <- unique(dat[["full_data"]][, input[["group"]]])

      ratios <- ratio_table() %>%
        mutate(across(groups, greater_eq_than_thresh, thresh = input[["remove_threshold"]]))

      grouped_variables <- lapply(groups, function(ith_group) {
        ratios %>%
          filter(get(ith_group)) %>%
          pull(variable)
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


  # imputation

  ## filter methods

  output[["n_methods"]] <- renderText({
    n <- length(input[["methods"]])

    paste0("Number of chosen methods: ", n)
  })


  observeEvent(input[["best"]], {
    best_methods <- pull(filter(methods_table, best), name)

    if(input[["best"]])
      updateMultiInput(session = session,
                       inputId = "methods",
                       selected = c(input[["methods"]], best_methods))
    else
      updateMultiInput(session = session,
                       inputId = "methods",
                       selected = setdiff(input[["methods"]], best_methods))
  }, ignoreInit = TRUE)


  observeEvent(input[["fastest"]], {
    fastest_methods <- pull(filter(methods_table, fastest), name)
    if(input[["fastest"]])
      updateMultiInput(session = session,
                       inputId = "methods",
                       selected = c(input[["methods"]], fastest_methods))
    else
      updateMultiInput(session = session,
                       inputId = "methods",
                       selected = setdiff(input[["methods"]], fastest_methods))
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

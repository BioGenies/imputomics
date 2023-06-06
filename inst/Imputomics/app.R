library(shiny)
library(shinythemes)
library(DT)
library(readxl)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(tidyr)


source("supp.R")

ui <- navbarPage(
  theme = shinytheme("sandstone"),
  title = "Imputomics",
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
                              selected = "zero")
           ),
           column(6,
                  align = "center",
                  offset = 1,
                  h2("Dataset preview:"),
                  DT::dataTableOutput("missing_data")
           )
  ),
  tabPanel("Visualization",
           column(3,
                  h4("Count Table:"),
                  tableOutput("mv_pctg"),
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
           column(8,
                  offset = 1,
                  style = 'border-left: 1px solid',
                  column(12, align = "center", plotOutput("mv_vis"))

           ),

  ),
  tabPanel("Imputation"),
  tabPanel("Summary")
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
                                 pageLength = 15,
                                 searching = FALSE))
  })

  ##### data vis

  output[["mv_pctg"]] <- renderTable({
    req(dat[["missing_data"]])

    tmp_dat <- data.frame(
      Variable = colnames(dat[["missing_data"]]),
      Percentage_Missing = 100*colMeans(is.na(dat[["missing_data"]]))
    )%>%
      arrange(-Percentage_Missing) %>%
      rename(`% Missing` = Percentage_Missing)

    dat[["mv_summary"]] <- tmp_dat

    tmp_dat %>%
      mutate(missing = `% Missing` > 0) %>%
      mutate(missing = ifelse(missing,
                              "missing variables",
                              "complete variables")) %>%
      group_by(missing) %>%
      summarise(n = n())
  }, colnames = FALSE)


  output[["mv_vis"]] <- renderPlot({
    req(dat[["mv_summary"]])
    req(dat[["missing_data"]])

    if(input[["plot_type"]] == "Percentage") {
      tmp_dat <- dat[["mv_summary"]]
      if(!input[["show_non_miss"]])
        tmp_dat <- filter(tmp_dat, `% Missing` > 0)
      plot_mv_segment(tmp_dat)
    } else {
      tmp_dat <- dat[["missing_data"]]
      if(!input[["show_non_miss"]])
        tmp_dat <- select(tmp_dat, where(function(x) any(is.na(x))))
      plot_mv_heatmap(tmp_dat)
    }
  })

}

shinyApp(ui, server)

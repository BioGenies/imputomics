library(shiny)
library(dplyr)

number_impute <- function(missing_data, number){
  missing_data[is.na(missing_data)] <- number
  missing_data
}
imputation_method <- lapply(0:20, function(ith_number){
  function(missing_data){number_impute(missing_data, number = ith_number)}
}) %>%
  setNames(paste0('impute_', 0:20))

navbarPage(id = 'tabs',
           title = 'Nazwa apki',
           tabPanel("Upload data file",
                    fileInput(inputId = 'missing_data',
                              label = "Wybierz plik z brakujacymi wartosciami",
                              multiple = FALSE,
                              accept = c(".csv", ".xlsx")),#,  dodac wiecej rozszerzen
                    tableOutput("table")),
           tabPanel("Metody imputacji",
                    actionButton(inputId = "id_select_all", label = "Select all"),
                    actionButton(inputId = "id_deselect_all", label = "Deselect all"),
                    checkboxGroupInput(inputId = 'id_imput_metod',
                                       label = "Wybierz imputacje",
                                       choices = names(imputation_method),
                                       selected = NULL)),
           tabPanel("Download",
                    actionButton("id_make", label = "wykonaj analizy"),
                    radioButtons(inputId = 'id_save',
                                 label = "w czym zapisac",
                                 choices = c('Excel', 'csv'),
                                 selected = 'Excel'),
                    actionButton("download",
                                   "Pobierz plik"))
)

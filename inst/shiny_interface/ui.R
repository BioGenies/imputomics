#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
navbarPage(id = 'tabs',
           title = 'Nazwa apki',
           tabPanel("Upload data file",
                    fileInput(inputId = 'missing_data',
                              label = "Wybierz plik z brakujacymi wartosciami",
                              multiple = FALSE,
                              accept = c(".csv", ".xlsx", ".xls"))), # dodac wiecej rozszerzen
           tabPanel("Metody imputacji",
                    checkboxGroupInput(inputId = 'id_imput_metod',
                                       label = "Wybierz imputacje",
                                       choices = c("A" = 'A', "b" = 'B'),
                                       selected = 'A')),
           tabPanel("Download",
                    radioButtons(inputId = 'id_save',
                                 label = "w czym zapisac",
                                 choices = c('Excel', 'csv'),
                                 selected = 'Excel'),
                    downloadButton("Download",
                                   "Pobierz plik")),
           tabPanel("Dziala",
                    numericInput("bins",
                                 label = 'Liczba',
                                 value = 5,
                                 min = 1,
                                 max = 10,
                                 step = 1))
           )

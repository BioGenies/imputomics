library(shiny)
library(readxl)
# library(xlsx)# nie dziala u mnie
library(openxlsx)
library(dplyr)

number_impute <- function(missing_data, number){
  missing_data[is.na(missing_data)] <- number
  missing_data
}
imputation_method <- lapply(0:20, function(ith_number){
  function(missing_data){number_impute(missing_data, number = ith_number)}
}) %>%
  setNames(paste0('impute_', 0:20))

function(input, output, session) {

  observeEvent(input[['id_select_all']], {
    updateCheckboxGroupInput(session,"id_imput_metod","Wybierz imputacje",selected = names(imputation_method))
  })

  observeEvent(input[['id_deselect_all']], {
    updateCheckboxGroupInput(session,"id_imput_metod","Wybierz imputacje", selected = c(''))
  })

  data_upload <- reactive({
    file <- input[['missing_data']]
    ext <- tools::file_ext(file[['datapath']])

    switch(ext,
           xlsx = {
             # sheets[['sheet']] <- readxl::excel_sheets(file[["datapath"]])
             dat <- readxl::read_excel(file[["datapath"]], na = 'NA') # moze jeszcze arkusze
           },
           csv = {}
    )
    dat
  })

  output[['table']] <- renderTable({
    dat_impute()[[1]]
  })

  dat_impute <- reactive({ # trzeba zamienic na observEvent chyba jakos
    input[['id_make']]
    lapply(input[['id_imput_metod']], function(ith_imput){
      imputation_method[[ith_imput]](data_upload())
    }) %>%
      setNames(input[['id_imput_metod']])
  })

  observeEvent(input[['download']], {
    switch(input[['id_save']],
           Excel = {
             t <- createWorkbook()
             for(i in names(dat_impute())){
               addWorksheet(t, i)
               writeData(t,i,dat_impute()[[i]])
             }
             saveWorkbook(t, "test.xlsx",overwrite = TRUE)
           },
           csv = {
             write.csv(dat_impute()[[1]],
                       file = 'rest.csv',
                       row.names = FALSE)
           }
    )
  })

}

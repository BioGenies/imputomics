
round_numeric <- function(dat) {
  dat %>%
    mutate_if(is.numeric, round, digits = 3) %>%
    relocate(where(is.numeric), .after = last_col())
}


greater_eq_than_thresh <- function(column, thresh) {
  column >= thresh
}



validate_data <- function(uploaded_data, session, input) {

  # check if data is empty
  if(is.null(uploaded_data)){
    sendSweetAlert(session = session,
                   title = "No data!",
                   text = "Your data is empty or the file is broken.
                   Make sure that the uploaded file contains dataset with numeric columns.",
                   type = "error")
    uploaded_data <- NULL
  } else {

    # check if the columns are numeric
    non_numeric_cols <- !sapply(uploaded_data, is.numeric)
    uploaded_data <- uploaded_data[, sapply(uploaded_data, is.numeric)]

    if(any(non_numeric_cols) & !ncol(uploaded_data) == 0) {
      showNotification(paste0("Your data contains ", sum(non_numeric_cols),
                              " non-numeric columns! We will ignore them during imputation!"),
                       session = session,
                       type = "warning",
                       duration = 20)
    }

    if(ncol(uploaded_data) == 0) {
      sendSweetAlert(session = session,
                     title = "No numeric columns!",
                     text = "Make sure that the uploaded file contains dataset with numeric columns.",
                     type = "error")
      uploaded_data <- NULL
    } else {
      ncol_numeric <- sum(sapply(uploaded_data, is.numeric))
      if(ncol_numeric < 5)
        showNotification(paste0("You provided data with only ", ncol_numeric,
                                " numeric columns. Some methods may not work properly."),
                         session = session,
                         type = "error",
                         duration = 20)

      if(nrow(uploaded_data) < 5)
        showNotification(paste0("Your data has only ", nrow(uploaded_data), " rows.",
                                "Some methods may not work properly."),
                         session = session,
                         type = "error",
                         duration = 20)
    }
  }
  uploaded_data
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
    mutate(name_fun = name,
           name = paste0(name, " (", full_name, ")"))

  funs_imputomics <- ls("package:imputomics")
  funs_imputomics <- funs_imputomics[grepl("impute_", funs_imputomics)]

  methods_table %>%
    filter(imputomics_name %in% funs_imputomics)
}


save_excel <- function(dat, file, download_methods) {
  wb_file <- createWorkbook()
  addWorksheet(wb_file, "original_data")
  writeData(wb_file, "original_data", dat[["uploaded_data"]], colNames = TRUE)

  result_data <- dat[["results"]][["results"]]

  methods <- dat[["results"]][["success"]] %>%
    filter(name %in% download_methods) %>%
    pull(imputomics_name)

  result_data <- result_data[methods]
  methods <- str_replace_all(str_remove(names(result_data),"impute_"), "_", " ")
  for (i in 1:length(result_data)) {
    if(nrow(result_data[[i]]) != 0) {
      addWorksheet(wb_file, methods[i])
      method_i_full_data <- dat[["uploaded_data"]]
      method_i_full_data[, colnames(result_data[[i]])] <- result_data[[i]]
      method_i_full_data <- method_i_full_data %>% dplyr::select(-dat[["removed"]])
      writeData(wb_file, methods[i], method_i_full_data, colNames = TRUE)
    }
  }
  saveWorkbook(wb_file, file, overwrite = TRUE)
}

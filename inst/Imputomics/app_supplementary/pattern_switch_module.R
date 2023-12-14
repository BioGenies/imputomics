
add_methods_UI <- function(id) {
  ns <- NS(id)
  content <- switch (id,
                     fastest = "Add the 10 fastest methods.",
                     best = "Add the 10 most accurate methods.",
                     MCAR = "Add the 5 most accurate methods for MCAR.",
                     MAR = "Add the 5 most accurate methods for MAR.",
                     MNAR = "Add the 5 most accurate methods for MNAR.")
  tagList(
    prettySwitch(
      inputId = ns("methods_switch"),
      label = content,
      status = "danger",
      value = FALSE,
      slim = TRUE
    ),
  )
}



add_methods_SERVER <- function(id, global_input, parent, methods_table) {

  moduleServer(id, function(input, output, session) {

    observeEvent(input[["methods_switch"]], {
      best_methods <- pull(filter(methods_table, get(id)), name)
      if (input[["methods_switch"]])
        updated_methods <- c(global_input[["methods"]], best_methods)
      else
        updated_methods <- setdiff(global_input[["methods"]], best_methods)

      updateMultiInput(session = parent,
                       inputId = "methods",
                       selected = updated_methods)
    }, ignoreInit = TRUE)
  })
}



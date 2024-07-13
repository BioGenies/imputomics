
download_plot_UI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyWidgets::dropdownButton(
      inputId = id,
      tagList(
        h4("Plot settings:"),
        br(),
        numericInput(ns("plot_h"),
                     label = "Plot height [inch]:",
                     value = 7, min = 1, max = 20),
        numericInput(ns("plot_w"),
                     label = "Plot width [inch]:",
                     value = 14, min = 1, max = 20),
        br(),
        column(12,
               shinyWidgets::downloadBttn(ns("download_plot"),
                            block = TRUE,
                            label = "Download",
                            color = "primary")),
        br(),
        br()
      ),
      circle = TRUE, status = "primary", right = TRUE,
      icon = icon("download"), width = "300px",
      tooltip = shinyWidgets::tooltipOptions(title = "Click to download!",
                               placement = "left")
    ),
  )
}



download_plot_SERVER <- function(id,
                                 plot_reactive,
                                 point_plot_dat = NULL) {
  moduleServer(id, function(input, output, session) {

    output[["download_plot"]] <- downloadHandler(
      filename = function() {
        paste0(switch(id,
                      points = point_plot_dat(),
                      segment = "percentage_missing_values",
                      heatmap = "missing_value_heatmap"), ".pdf")
      },
      content = function(file) {
        pdf(file, width = input[["plot_w"]], height = input[["plot_h"]])
        plot(plot_reactive())
        dev.off()
      }
    )

  })
}

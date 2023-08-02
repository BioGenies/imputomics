#' Imputomics Graphical User Interface
#'
#' @description Launches Imputomics.
#'
#' @importFrom shiny runApp
#'
#' @param port The TCP port. See \code{\link[shiny]{runApp}}.
#'
#' @section Warning : Any ad-blocking software may cause malfunctions.
#'
#' @export imputomics_gui

imputomics_gui <- function(port = getOption("shiny.port"))
  runApp(system.file("Imputomics", package = "imputomics"), port = port, launch.browser = TRUE)

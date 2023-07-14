#' imputomics Graphical User Interface
#'
#' @description Launches graphical user interface .
#'
#' @importFrom tools package_dependencies
#' @importFrom utils available.packages
#' @importFrom shiny runApp
#'
#' @section Warning: Any ad-blocking software may cause malfunctions.
#'
#' @author "Michał Burdukiewicz", "Krystyna Grzesiak", "Dominik Nowakowski",
#' "Jarosław Chilimoniuk"
#'
#' @examples
#' if(interactive()) {
#'   imputomics()
#' }
#'
#' @keywords 
#' @export imputomics_gui
#'

imputomics_gui <- function(){
  imputomics_suggests <- package_dependencies("imputomics",
                                               which = "Suggests")[["imputomics"]]

  if(!all(imputomics_suggests %in% rownames(available.packages()))) {
    stop(paste0("The imputomics GUI requires following packages: ",
                paste0(imputomics_suggests, collapse = ", ")))
  }
  runApp(system.file("Imputomics", package = "imputomics"))
}

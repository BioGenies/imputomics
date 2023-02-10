#' imputomics Graphical User Interface
#'
#' @description Launches graphical user interface .
#' @name imputomics_gui
#' @section Warning: Any ad-blocking software may cause malfunctions.
#' @keywords count Poisson zero-inflated
#' @export imputomics_gui
#' @importFrom tools package_dependencies
#' @importFrom utils available.packages
#' @author
#' @examples
#' if(interactive()) {
#'   imputomics()
#' }

imputomics_gui <- function(){
  imputomics_suggests <- package_dependencies("imputomics",
                                               which = "Suggests")[["imputomics"]]

  if(!all(imputomics_suggests %in% rownames(available.packages()))) {
    stop(paste0("The imputomics GUI requires following packages: ",
                paste0(imputomics_suggests, collapse = ", ")))
  }
  runApp(system.file("imputomics", package = "imputomics"))
}
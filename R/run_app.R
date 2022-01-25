#' Launches the shiny app to generate reports.
#'
#' @return NULL
#'
#' @export
#' @import ruODK shiny rmarkdown shinyalert DT dplyr shinycssloaders
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
run_app <- function() {

  appDir <- system.file("app", package = "repvisforODK")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

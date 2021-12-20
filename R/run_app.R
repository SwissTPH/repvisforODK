#' Launches the shiny app to generate reports.
#'
#' @return NULL
#'
#' @export
#'
#' @examples
run_app <- function() {

  shinyApp(repvisforODK::ui(), repvisforODK::server)
}

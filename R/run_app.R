#' Launches the shiny app that users can use to do generate reports.
#'
#' @return NULL
#'
#' @export
#'
#' @examples
run_app <- function() {
  shinyApp(repvisforODK::ui(), repvisforODK::server)
}

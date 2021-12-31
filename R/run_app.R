#' Launches the shiny app to generate reports.
#'
#' @return NULL
#'
#' @export
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
run_app <- function() {

  shinyApp(repvisforODK::ui(), repvisforODK::server)
}

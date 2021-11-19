#' Server code for the shiny app that users can use to do generate reports.
#'
#' @param input
#' @param output
#'
#' @return
#'
#' @export
#' @import ruODK shiny rmarkdown
#'
#' @examples
server <- function(input, output) {

  df <- shiny::eventReactive(input$load_render_button, {

    if (input$data_source == 'csv') {

      shiny::req(input$csv_file)

      df <- read.csv(input$csv_file$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)

    } else if (input$data_source == 'svc') {

      shiny::req(input$svc_text)
      shiny::req(input$un)
      shiny::req(input$pw)
      shiny::req(input$tz)

      repvisforODK::setup_ruODK(svc = input$svc_text, un = input$un, pw = input$pw, tz = input$tz)

      df <- ruODK::odata_submission_get(download = FALSE)
    }
  })

  df_schema <- shiny::eventReactive(input$load_render_button, {

    if (input$data_source == 'svc') {

      shiny::req(input$svc_text)
      shiny::req(input$un)
      shiny::req(input$pw)
      shiny::req(input$tz)

      df_schema <- ruODK::form_schema_ext()
    }
  })

  output$contents <- renderDataTable({
    shiny::req(df())

    df()
  })

  shiny::observeEvent(input$next1, {
    shiny::updateTabsetPanel(inputId = "tab",
                      selected = '2. Select Visualisations')
  })

  shiny::observeEvent(input$next2, {
    shiny::updateTabsetPanel(inputId = "tab",
                      selected = '3. Set Parameters')
  })

  shiny::observeEvent(input$prev1, {
    shiny::updateTabsetPanel(inputId = "tab",
                      selected = '1. Select Data')
  })

  shiny::observeEvent(input$prev2, {
    shiny::updateTabsetPanel(inputId = "tab",
                      selected = '2. Select Visualisations')
  })

  output$data_flag <- reactive(
    if (nrow(df()) > 0) TRUE else FALSE
    )
  outputOptions(output, "data_flag", suspendWhenHidden = FALSE)

  output$lang_flag <- reactive(
    if (TRUE %in% grepl("label_\\w*", colnames(df_schema()))) TRUE else FALSE
  )
  outputOptions(output, "lang_flag", suspendWhenHidden = FALSE)

  output$report <- shiny::downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(n = input$slider)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

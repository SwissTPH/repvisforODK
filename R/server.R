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

  df <- shiny::eventReactive(input$load_preview_button, {

    shiny::req(input$svc_text)
    shiny::req(input$un)
    shiny::req(input$pw)
    shiny::req(input$tz)

    repvisforODK::setup_ruODK(svc = input$svc_text, un = input$un, pw = input$pw, tz = input$tz)

    df <- ruODK::odata_submission_get(download = FALSE)

  })

  df_schema <- shiny::eventReactive(input$load_preview_button, {

    shiny::req(input$svc_text)
    shiny::req(input$un)
    shiny::req(input$pw)
    shiny::req(input$tz)

    df_schema <- ruODK::form_schema_ext()

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

  output$report_button <- shiny::downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "repvis_report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      report_path <- system.file('rmarkdown', 'all_plots_shiny.rmd', package = 'repvisforODK')

      # Set up parameters to pass to Rmd document
      params <- list(title = input$title_param,
                     author = input$author_param,
                     date_col = input$date_col_param,
                     daily_submission_goal = input$sub_goal_param,
                     exclude_weekend = input$exclude_weekend_param,
                     delimiter = input$delimiter_param,
                     lang = input$lang_param,
                     lang_wc = input$lang_wc_param,
                     text_col = input$text_col_param,
                     plots_general = input$general_plots,
                     plots_question = input$question_plots)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(input = report_path,
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

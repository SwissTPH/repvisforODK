server <- function(input, output) {

  df <- eventReactive(input$load_render_button, {

    if (input$data_source == 'csv') {

      req(input$csv_file)

      df <- read.csv(input$csv_file$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)

    } else if (input$data_source == 'svc') {

      req(input$svc_text)
      req(input$un)
      req(input$pw)
      req(input$tz)

      repvisforODK::setup_ruODK(svc = input$svc_text, un = input$un, pw = input$pw, tz = input$tz)

      df <- ruODK::odata_submission_get(download = FALSE)
    }
  })

  df_schema <- reactive({

    if (input$data_source == 'svc') {

      req(input$svc_text)
      req(input$un)
      req(input$pw)
      req(input$tz)

      df_schema <- ruODK::form_schema_ext()
    }
  })

  output$contents <- renderDataTable({
    req(df())

    df()
  })



  output$report <- downloadHandler(
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
)

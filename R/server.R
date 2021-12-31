#' Server code for the shiny app that users can use to do generate reports.
#'
#' Standard Shiny app component.
#'
#' @param input Shiny server input param
#' @param output Shiny server output param
#'
#' @return
#'
#' @export
#' @import ruODK shiny rmarkdown shinyalert DT
server <- function(input, output) {

  # suppress warnings; = 0 to enable warnings
  options(warn = -1)

  # main data-------------------------------------------------------------------------------------------------------------------------

  # loading data when ODK credentials are specified and load_preview_button is pressed
  df <- shiny::eventReactive(input$load_preview_button, {

    shiny::req(input$svc_text)
    shiny::req(input$un)
    shiny::req(input$pw)
    shiny::req(input$tz)

    repvisforODK::setup_ruODK(svc = input$svc_text, un = input$un, pw = input$pw, tz = input$tz)

    df <- repvisforODK::load_data_sub_date(tz = 'Europe/Berlin')

  })

  # loading form schema
  df_schema <- shiny::eventReactive(input$load_preview_button, {

    shiny::req(input$svc_text)
    shiny::req(input$un)
    shiny::req(input$pw)
    shiny::req(input$tz)

    df_schema <- ruODK::form_schema_ext()

  })

  # date filter-------------------------------------------------------------------------------------------------------------------------

  # subset-ting data using user-defined date range
  df_fin <- shiny::reactive({

    shiny::req(df())

    if (input$filter_check == TRUE) {
      df()[as.Date(df()[[input$filter_col]]) >= input$date_range[1] & as.Date(df()[[input$filter_col]]) <= input$date_range[2], ]
    } else df()
  })

  # get data collection period of data for date range defaults
  collection_period <- shiny::reactive({

    shiny::req(df())
    shiny::req(input$filter_col)

    repvisforODK::collection_period(date_col = input$filter_col, df = df())
  })


  shiny::observe({

    shiny::req(collection_period())

    updateDateRangeInput(inputId = 'date_range',
                         start = collection_period()[[1]],
                         end = collection_period()[[2]])
  })

  # daily sub goal mandatory for donuts-------------------------------------------------------------------------------------------------------------------------

  shiny::observe({

    shiny::req(input$general_plots)
    shiny::req(input$sub_goal_param)

    if ('donut' %in% input$general_plots) {
      shiny::updateNumericInput(inputId = 'sub_goal_param',
                         label = 'Daily submission goal*')

      shiny::updateCheckboxInput(inputId = 'sub_goal_check',
                                 label = 'Include daily submission goal in general plots*',
                                 value = TRUE)
    } else {
      shiny::updateNumericInput(inputId = 'sub_goal_param',
                         label = 'Daily submission goal')

      shiny::updateCheckboxInput(inputId = 'sub_goal_check',
                                 label = 'Include daily submission goal in general plots',
                                 value = FALSE)
    }
  })

  # pre-filtering potential free text columns-------------------------------------------------------------------------------------------------------------------------

  text_col_choices <- shiny::reactive({

    shiny::req(df_schema())
    shiny::req(input$label_col_param)
    shiny::req(input$choice_col_param)

    # excluding all questions that have choices, are not ODK type 'string', are not NA, contain 'generated_' in their question name
    df_schema()[[input$label_col_param]][df_schema()$type == 'string' & !df_schema()$ruodk_name %in% repvisforODK::identify_choice_questions(df_schema_ext = df_schema(), label_col = input$label_col_param, choice_col = input$choice_col_param)[[2]] & !grepl("generated_", df_schema()$ruodk_name) & !is.na(df_schema()[[input$label_col_param]])]

  })

  shiny::observe({

    shiny::req(text_col_choices())

    shiny::updateSelectInput(inputId = 'text_col_param',
                      choices = text_col_choices())
  })

  # datetime, label, and choice column pre-filtering-------------------------------------------------------------------------------------------------------------------------

  shiny::observeEvent(input$load_preview_button, {

    # get all columns that have class POSIXct or POSIXlt
    datetime_col_choices <- colnames(df() %>% dplyr::select_if(function(col) is.POSIXct(col) | is.POSIXlt(col)))
    shiny::updateRadioButtons(inputId = 'filter_col',
                       choices = datetime_col_choices)
    shiny::updateSelectInput(inputId = 'date_col_param',
                       choices = datetime_col_choices)

    # get all columns that start with the word 'label'
    label_col_choices <- colnames(df_schema())[grepl("label\\w*", colnames(df_schema()))]
    label_col_choices_fin <- c()
    for (col in label_col_choices) {
      if (sum(is.na(df_schema()[[col]])) > nrow(df_schema())-2) {
        next
      } else label_col_choices_fin <- c(label_col_choices_fin, col)
    }
    shiny::updateSelectInput(inputId = 'label_col_param',
                       choices = label_col_choices_fin)

    # get all columns that start with the word 'choice'
    choice_col_choices <- colnames(df_schema())[grepl("choices\\w*", colnames(df_schema()))]
    choice_col_choices_fin <- c()
    for (col in choice_col_choices) {
      if (sum(is.na(df_schema()[[col]])) > nrow(df_schema())-2) {
        next
      } else choice_col_choices_fin <- c(choice_col_choices_fin, col)
    }
    shiny::updateSelectInput(inputId = 'choice_col_param',
                       choices = choice_col_choices_fin)
  })

  # DT datatable-------------------------------------------------------------------------------------------------------------------------

  output$contents <- DT::renderDT({
    shiny::req(df_fin())

    DT::datatable(df_fin(),
                  class = 'cell-border stripe',
                  filter = 'top',
                  options = list(pageLength = 10))

  })

  # panel changes through navigation buttons-------------------------------------------------------------------------------------------------------------------------

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

  # event flags for determining appearance of conditional panels-------------------------------------------------------------------------------------------------------------------------

  output$data_flag <- shiny::reactive(
    if (nrow(df()) > 0) TRUE else FALSE
    )
  shiny::outputOptions(output, "data_flag",
                       suspendWhenHidden = FALSE)

  output$lang_flag <- shiny::reactive(
    if (TRUE %in% grepl("label_\\w*", colnames(df_schema()))) TRUE else FALSE
  )
  shiny::outputOptions(output, "lang_flag",
                       suspendWhenHidden = FALSE)

  # download of rmd report-------------------------------------------------------------------------------------------------------------------------

  # Create reactiveValues object and set flag to 0 to prevent errors with adding NULL
  rv <- shiny::reactiveValues(download_flag = 0)

  output$report_button <- shiny::downloadHandler(

    filename = "repvis_report.html",
    content = function(file) {

      # When the downloadHandler function runs, increment rv$download_flag
      rv$download_flag <- rv$download_flag + 1

      if(rv$download_flag > 0){  # trigger event whenever the value of rv$download_flag changes
        shinyalert::shinyalert(
          title = 'Your report is on the way!',
          text = 'Usually the creation only takes some seconds.\n Depending on data size and download speed it can be more...',
          size = 's',
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "success",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = " #bf3227",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
      }

      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      report_path <- system.file('rmarkdown', 'all_plots_shiny.rmd', package = 'repvisforODK')

      # Set up parameters to pass to Rmd document
      params <- list(title = input$title_param,
                     author = input$author_param,
                     df = df_fin()[input$contents_rows_all, ],
                     df_schema = df_schema(),
                     svc = input$svc_text,
                     date_col = input$date_col_param,
                     daily_submission_goal = input$sub_goal_param,
                     exclude_wday_str = input$exclude_wday_param,
                     delimiter = input$delimiter_param,
                     lang_wc = tolower(input$lang_wc_param),
                     text_col = df_schema()$ruodk_name[df_schema()[[input$label_col_param]] %in% input$text_col_param],
                     text_col_name = input$text_col_param,
                     choice_col = input$choice_col_param,
                     label_col = input$label_col_param,
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

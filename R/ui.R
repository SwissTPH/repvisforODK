#' UI code for the shiny app that users can use to do generate reports.
#'
#' @return
#'
#' @export
#'
#' @examples
ui <- function() {
  ui <- shiny::navbarPage('repvisforODK', id = 'tab',
                          shiny::tabPanel('1. Select Data',
                            shiny::sidebarLayout(
                              shiny::sidebarPanel(

                                # Input: Select data source
                                shiny::radioButtons(inputId = 'data_source',
                                             label = 'Select data source:',
                                             choices = c('Directly from ODK (ODATA)' = 'svc',
                                                         'CSV file' = 'csv')
                                ),

                                # Horizontal line
                                tags$hr(),

                                shiny::conditionalPanel(
                                  condition = 'input.data_source == "csv"',

                                  # Input: Select a file
                                  shiny::fileInput("csv_file", "Choose CSV File",
                                            multiple = TRUE,
                                            accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                       ".csv")
                                  ),

                                  # Horizontal line
                                  tags$hr(),

                                  # Input: Checkbox if file has header
                                  shiny::checkboxInput("header", "Header", TRUE),

                                  # Input: Select separator
                                  shiny::radioButtons("sep", "Separator",
                                               choices = c(Comma = ",",
                                                           Semicolon = ";",
                                                           Tab = "\t"),
                                               selected = ","),

                                  # Input: Select quotes
                                  shiny::radioButtons("quote", "Quote",
                                               choices = c(None = "",
                                                           "Double Quote" = '"',
                                                           "Single Quote" = "'"),
                                               selected = '"')

                                ),

                                shiny::conditionalPanel(
                                  condition = 'input.data_source == "svc"',

                                  # Input: Enter svc link
                                  shiny::textInput(inputId = 'svc_text',
                                            label = 'SVC',
                                            placeholder = 'https://research.odk.path.org/#/projects/projectNumber/forms/projectName/submissions'),

                                  # Input: Enter username
                                  shiny::textInput(inputId = 'un',
                                            label = 'Username',
                                            placeholder = 'lucas.silbernagel@swisstph.ch'),

                                  # Input: Enter password
                                  shiny::passwordInput(inputId = 'pw',
                                                label = 'Password',
                                                placeholder = 'S3cur3_Password123'),

                                  # Input: Enter timezone
                                  shiny::textInput(inputId = 'tz',
                                            label = 'Timezone',
                                            placeholder = "Europe/Berlin"),
                                ),

                                # Horizontal line
                                tags$hr(),

                                shiny::actionButton("load_render_button", "Load and Preview Data"),

                                # Horizontal line
                                tags$hr(),

                                shiny::actionButton('next1', 'Next'),

                              ),

                              shiny::mainPanel(
                                # Output: Preview data file
                                shiny::dataTableOutput("contents")
                              )


                            )
                   ),

                   shiny::tabPanel('2. Select Visualisations',
                            shiny::sidebarLayout(
                              shiny::sidebarPanel(

                                # Input: Select data source
                                shiny::selectInput(inputId = 'general_plots',
                                            label = 'Select general plots:',
                                            choices = c('Daily Submission Goal Donut' = 'donut',
                                                        'Submissions Over Time Line Chart (Cumulative)' = 'line_chart_cumsum',
                                                        'Submissions Over Time Line Chart (Non-Cumulative)' = 'line_chart_no_cumsum',
                                                        'Day of Week / Time of Day Heat Map' = 'day_heatmap',
                                                        'Calendar Heat Map' = 'cal_heatmap'),
                                            multiple = TRUE
                                ),

                                # Horizontal line
                                tags$hr(),

                                shiny::selectInput(inputId = 'question_plots',
                                            label = 'Select question-specific plots:',
                                            choices = c('Single Choice Question Pie Chart' = 'single_pie',
                                                        'Multiple Choice Question Bar Chart' = 'multiple_bar',
                                                        'Free Text Question Word Cloud' = 'wordcloud',
                                                        'Free Text Question Word Frequency Table' = 'freq_table'),
                                            multiple = TRUE
                                ),

                                # Horizontal line
                                tags$hr(),

                                shiny::actionButton('prev1', 'Previous'),
                                shiny::actionButton('next2', 'Next')


                              ),

                              shiny::mainPanel(
                                shiny::tabsetPanel(type = 'tab',
                                            shiny::tabPanel('Examples', tags$iframe(style = 'height:400px; width:100%; scrolling=yes',
                                                                             src = 'https://lucidviews.github.io/repvisforODK/docs/example_plots_repvis.pdf')))
                              )

                            )),

                   shiny::tabPanel('3. Set Parameters',
                            shiny::downloadButton("report", "Generate report"),

                            # Horizontal line
                            tags$hr(),

                            shiny::actionButton('prev2', 'Previous'))



  )


  return(ui)
}


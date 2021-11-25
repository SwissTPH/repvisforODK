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
                                             label = 'Data source:',
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
                                            label = 'SVC:',
                                            placeholder = 'https://research.odk.path.org/#/projects/projectNumber/forms/projectName/submissions',
                                            value = 'https://research.odk.path.org/v1/projects/4/forms/02-TIMCI-SPA-cgei.svc'),

                                  # Input: Enter username
                                  shiny::textInput(inputId = 'un',
                                            label = 'Username:',
                                            placeholder = 'lucas.silbernagel@swisstph.ch',
                                            value = 'lucas.silbernagel@swisstph.ch'),

                                  # Input: Enter password
                                  shiny::passwordInput(inputId = 'pw',
                                                label = 'Password:',
                                                placeholder = 'S3cur3_Password123',
                                                value = 'Sturm_66666613359'),

                                  # Input: Enter timezone
                                  shiny::textInput(inputId = 'tz',
                                            label = 'Timezone:',
                                            placeholder = "Europe/Berlin",
                                            value = 'Europe/Berlin'),
                                ),

                                # Horizontal line
                                tags$hr(),

                                shiny::actionButton("load_render_button", "Load and Preview Data"),

                                shiny::conditionalPanel(
                                  condition = 'output.data_flag == true',

                                  # Horizontal line
                                  tags$hr(),

                                  shiny::actionButton('next1', 'Next')
                                ),

                              ),

                              shiny::mainPanel(
                                # Output: Preview data file
                                shiny::dataTableOutput("contents")
                              )
                            )
                   ),

                   shiny::tabPanel('2. Select Visualisations',

                                   shiny::conditionalPanel(
                                     condition = 'output.data_flag != true',
                                     tags$h3('No data available from Step 1. Please go back and either upload a csv file or provide valid ODK Central credentials for an ODK form.')
                                     ),

                                    shiny::conditionalPanel(
                                      condition = 'output.data_flag == true',
                                      shiny::sidebarLayout(
                                        shiny::sidebarPanel(

                                          # Input: Select data source
                                          shiny::checkboxGroupInput(inputId = 'general_plots',
                                                      label = 'Select general plots:',
                                                      choices = c('Daily Submission Goal Donut' = 'donut',
                                                                  'Submissions Over Time Line Chart (Cumulative)' = 'line_chart_cumsum',
                                                                  'Submissions Over Time Line Chart (Non-Cumulative)' = 'line_chart_no_cumsum',
                                                                  'Day of Week / Time of Day Heat Map' = 'day_heatmap',
                                                                  'Calendar Heat Map' = 'cal_heatmap')
                                          ),

                                          # Horizontal line
                                          tags$hr(),

                                          shiny::checkboxGroupInput(inputId = 'question_plots',
                                                      label = 'Select question-specific plots:',
                                                      choices = c('Single Choice Question Pie Chart' = 'single_pie',
                                                                  'Multiple Choice Question Bar Chart' = 'multiple_bar',
                                                                  'Free Text Question Word Cloud' = 'wordcloud',
                                                                  'Free Text Question Word Frequency Table' = 'freq_table')
                                          ),

                                          # Horizontal line
                                          tags$hr(),

                                          shiny::actionButton('prev1', 'Previous'),

                                          shiny::conditionalPanel(
                                            condition = 'input.question_plots.length > 0 || input.general_plots.length > 0',

                                            shiny::actionButton('next2', 'Next')
                                          )
                                        ),

                                        shiny::mainPanel(

                                          shiny::conditionalPanel(
                                            condition = 'input.question_plots.length > 0 || input.general_plots.length > 0',

                                            tags$h3('Please select at least one visualisation before you proceed.')
                                          ),

                                          shiny::tabsetPanel(type = 'tab',
                                                      shiny::tabPanel('Examples', tags$iframe(style = 'height:400px; width:100%; scrolling=yes',
                                                                                       src = 'https://lucidviews.github.io/gh-pages/example_plots_repvis.pdf')
                                                                      )
                                                      )
                                          )
                                        )
                                      )
                             ),


                     shiny::tabPanel('3. Set Parameters',

                                     shiny::conditionalPanel(
                                       condition = 'input.question_plots.length == 0 && input.general_plots.length == 0',
                                       tags$h3('No visualisations selected in Step 2. Please select at least one visualisation.')
                                     ),

                                     shiny::conditionalPanel(
                                       condition = 'input.question_plots.length > 0 || input.general_plots.length > 0',

                                       # explain tab
                                       tags$h2('Based on the visualisations you selected, more parameters have to be defined.'),

                                       # line break
                                       tags$br(),

                                       shiny::conditionalPanel(
                                         condition = 'input.general_plots.length > 0 ',

                                         tags$h4('For the date column, it is important that your spelling is identical to the column name in the data. Common choices for this parameter are "start", "end", "system_submission_date" (SVC/ODATA) or "SubmissionDate" (CSV).'),

                                         # Input: Enter date column link
                                         shiny::textInput(inputId = 'date_col_param',
                                                          label = 'Enter date column:',
                                                          placeholder = 'e.g.: start'),

                                         # Horizontal line
                                         tags$hr(),

                                         # TODO: Implement logic for selection of donut (must be mandatory sub goal when selected)
                                         shiny::checkboxInput(inputId = 'sub_goal_check',
                                                              label = 'Tick the box if you want to include a submission goal in your general plots'),

                                         shiny::conditionalPanel(
                                           condition = 'input.sub_goal_check == true',

                                           shiny::numericInput(inputId = 'sub_goal',
                                                               label = 'Enter submission goal:',
                                                               value = 0)
                                         ),

                                         # Horizontal line
                                         tags$hr()
                                       ),

                                       shiny::conditionalPanel(
                                         condition = 'input.question_plots.length > 0 && output.lang_flag == true',

                                         # Input: Enter report language link
                                         shiny::textInput(inputId = 'lang_param',
                                                          label = 'Enter the langauge in which you want to translate question labels and choices to:',
                                                          placeholder = 'e.g.: english'),

                                         # Horizontal line
                                         tags$hr(),
                                       ),

                                       shiny::conditionalPanel(
                                         condition = 'input.question_plots.indexOf("multiple_bar") > -1',

                                         # Input: Enter report language link
                                         shiny::textInput(inputId = 'lang_param',
                                                          label = 'Please specify the delimiter with which the multiple choice question answers are separated:',
                                                          placeholder = 'e.g.: ,'),

                                         # Horizontal line
                                         tags$hr(),
                                       ),

                                       shiny::conditionalPanel(
                                         condition = 'input.general_plots.indexOf("donut") > -1 || input.general_plots.indexOf("line_chart_cumsum") > -1 || input.general_plots.indexOf("line_chart_no_cumsum") > -1',

                                         # Input: exclude weekend?
                                         shiny::checkboxInput(inputId = 'exclude_weekend',
                                                          label = 'Tick to not consider weekends for line chart(s) and/or donut chart',
                                                          value = FALSE)
                                         ),

                                       tags$br(),

                                       tags$br(),

                                       tags$br(),

                                       tags$br(),

                                       tags$br(),

                                       tags$br(),

                                       tags$br(),

                                       tags$br(),

                                       tags$br(),

                                       shiny::downloadButton("report", "Generate report"),

                                       # Horizontal line
                                       tags$hr(),

                                       shiny::actionButton('prev2', 'Previous')
                                       )
                                     )
                   )


  return(ui)
}


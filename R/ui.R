#' UI code for the shiny app that users can use to do generate reports.
#'
#' @return
#'
#' @export
#' @import shiny dplyr shinycssloaders
#'
#' @examples
ui <- function() {
  ui <- fluidPage(
    shiny::navbarPage(includeCSS('www/styles.css'),
                          title = 'repvisforODK',
                          id = 'tab',
                          selected = '1. Select Data',

                          shiny::tabPanel('1. Select Data',
                            shiny::sidebarLayout(
                              shiny::sidebarPanel(id = 'side1',

                                # Input: Enter svc link
                                  shiny::textInput(inputId = 'svc_text',
                                            label = 'SVC*',
                                            placeholder = 'https://research.odk.path.org/#/projects/projectNumber/forms/projectName/submissions',
                                            value = 'https://research.odk.path.org/v1/projects/4/forms/02-TIMCI-SPA-cgei.svc'),

                                  # Input: Enter username
                                  shiny::textInput(inputId = 'un',
                                            label = 'Username*',
                                            placeholder = 'lucas.silbernagel@swisstph.ch',
                                            value = 'lucas.silbernagel@swisstph.ch'),

                                  # Input: Enter password
                                  shiny::passwordInput(inputId = 'pw',
                                                label = 'Password*',
                                                placeholder = 'S3cur3_Password123'),

                                  # Input: Enter timezone
                                  shiny::textInput(inputId = 'tz',
                                            label = 'Timezone*',
                                            placeholder = "Europe/Berlin",
                                            value = 'Europe/Berlin'),

                                # line break
                                tags$br(),

                                tags$h6('*required'),

                                # Horizontal line
                                tags$hr(),

                                shiny::actionButton("load_preview_button", "Load and Preview Data"),

                                shiny::conditionalPanel(
                                  condition = 'output.data_flag == true',

                                  # Horizontal line
                                  tags$hr(),

                                  shiny::actionButton('next1', 'Next')
                                ),

                              ),

                              shiny::mainPanel(
                                # Output: Preview data file
                                shiny::dataTableOutput("contents") %>% shinycssloaders::withSpinner(color = '#bf3227')
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
                                        shiny::sidebarPanel(id = 'side2',

                                          # Input: Select data source
                                          shiny::checkboxGroupInput(inputId = 'general_plots',
                                                      label = 'Select general plots',
                                                      choices = c('Daily Submission Goal Donut' = 'donut',
                                                                  'Submissions Over Time Line Chart (Cumulative)' = 'line_chart_cumsum',
                                                                  'Submissions Over Time Line Chart (Non-Cumulative)' = 'line_chart_no_cumsum',
                                                                  'Day of Week / Time of Day Heat Map' = 'day_heatmap',
                                                                  'Calendar Heat Map' = 'cal_heatmap')
                                          ),

                                          # Horizontal line
                                          tags$hr(),

                                          shiny::checkboxGroupInput(inputId = 'question_plots',
                                                      label = 'Select question-specific plots',
                                                      choices = c('Single Choice Question Pie Chart' = 'single_pie',
                                                                  'Multiple Choice Question Bar Chart' = 'multiple_bar',
                                                                  'Free Text Question Word Cloud' = 'wordcloud')
                                          ),

                                          # Horizontal line
                                          tags$hr(),

                                          shiny::actionButton('prev1', 'Previous'),

                                          shiny::conditionalPanel(
                                            condition = 'input.question_plots.length > 0 || input.general_plots.length > 0',

                                            # Horizontal line
                                            tags$hr(),

                                            shiny::actionButton('next2', 'Next')
                                          )
                                        ),

                                        shiny::mainPanel(

                                          shiny::conditionalPanel(
                                            condition = 'input.question_plots.length == 0 && input.general_plots.length == 0',

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

                                       # for plots
                                       tags$h3('1. Plot Parameters'),

                                       shiny::conditionalPanel(
                                         condition = 'input.general_plots.length > 0 ',

                                         tags$h5('Spelling must be identical to the column name in the data. Common choices for this parameter: "start", "end", "system_submission_date" (SVC/ODATA) or "SubmissionDate" (CSV).'),

                                         # Input: Enter date column link
                                         shiny::textInput(inputId = 'date_col_param',
                                                          label = 'Enter date column*',
                                                          placeholder = 'e.g.: start'),

                                         # line break
                                         tags$br(),

                                         # TODO: Implement logic for selection of donut (must be mandatory sub goal when selected)
                                         shiny::checkboxInput(inputId = 'sub_goal_check',
                                                              label = 'Include submission goal in general plots'),

                                         shiny::conditionalPanel(
                                           condition = 'input.sub_goal_check == true',

                                           shiny::numericInput(inputId = 'sub_goal_param',
                                                               label = 'Enter daily submission goal*',
                                                               value = 1)
                                         ),

                                         # line break
                                         tags$br(),
                                       ),

                                       shiny::conditionalPanel(
                                         condition = 'input.question_plots.length > 0 && output.lang_flag == true',

                                         # TODO: Implement logic for selection of donut (must be mandatory sub goal when selected)
                                         shiny::checkboxInput(inputId = 'lang_check',
                                                              label = 'Form was created in multiple languages'),

                                         shiny::conditionalPanel(
                                           condition = 'input.lang_check == true',

                                           # Input: Enter report language link
                                           shiny::textInput(inputId = 'lang_param',
                                                            label = 'Enter the langauge in which you want to translate question labels and choices to*',
                                                            placeholder = 'e.g.: english'),
                                         ),

                                         # line break
                                         tags$br(),
                                       ),

                                       shiny::conditionalPanel(
                                         condition = 'input.question_plots.indexOf("multiple_bar") > -1',

                                         # Input: Enter report language link
                                         shiny::textInput(inputId = 'delimiter_param',
                                                          label = 'Please specify the delimiter with which the multiple choice question answers are separated*',
                                                          placeholder = 'e.g.: ,'),

                                         # line break
                                         tags$br(),
                                       ),

                                       shiny::conditionalPanel(
                                         condition = 'input.question_plots.indexOf("wordcloud") > -1',

                                         # Input: Enter report language link
                                         shiny::textInput(inputId = 'text_col_param',
                                                          label = 'Please specify the name of the question(s) you want to generate a word cloud for*',
                                                          placeholder = 'e.g.: j4_j4_2a'),

                                         # line break
                                         tags$br(),

                                         # Input: Enter report language link
                                         shiny::textInput(inputId = 'lang_wc_param',
                                                          label = 'Please specify the language of the answers to the(se) question(s)*',
                                                          placeholder = 'e.g.: english'),

                                         # line break
                                         tags$br(),
                                       ),

                                       shiny::conditionalPanel(
                                         condition = 'input.general_plots.indexOf("donut") > -1 || input.general_plots.indexOf("line_chart_cumsum") > -1 || input.general_plots.indexOf("line_chart_no_cumsum") > -1',

                                         # Input: exclude weekend?
                                         shiny::checkboxInput(inputId = 'exclude_weekend_param',
                                                          label = 'Tick to not consider weekends for line chart(s) and/or donut chart',
                                                          value = FALSE)
                                         ),

                                       # Horizontal line
                                       tags$hr(style = 'border-color: #337ab7;'),

                                       # for report
                                       tags$h3('2. Report Parameters'),

                                       # Input: Enter report language link
                                       shiny::textInput(inputId = 'title_param',
                                                        label = 'Please enter a title*',
                                                        placeholder = 'e.g.: timci_report_Nov_21'),

                                       # line break
                                       tags$br(),

                                       # Input: Enter report language link
                                       shiny::textInput(inputId = 'author_param',
                                                        label = 'Please enter your name (will be shown as author)*',
                                                        placeholder = 'e.g.: Lucas Silbernagel'),

                                       # line break
                                       tags$br(),

                                       tags$h6('*required'),

                                       # Horizontal line
                                       tags$hr(style = 'border-color: #337ab7;'),

                                       shiny::downloadButton("report_button", "Generate report"),

                                       # Horizontal line
                                       tags$hr(style = 'border-color: #337ab7;'),

                                       shiny::actionButton('prev2', 'Previous'),

                                       # line break
                                       tags$br()
                                       )
                                     )
                   ),

    div(class = "footer",
        includeHTML("html/footer.html")
       )
    )


  return(ui)
}


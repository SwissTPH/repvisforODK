ui <- navbarPage('repvisforODK',
                 tabPanel('1. Select Data',
                          sidebarLayout(
                            sidebarPanel(

                              # Input: Select data source
                              radioButtons(inputId = 'data_source',
                                           label = 'Select data source:',
                                           choices = c('Directly from ODK (ODATA)' = 'svc',
                                                       'CSV file' = 'csv')
                              ),

                              # Horizontal line
                              tags$hr(),

                              conditionalPanel(
                                condition = 'input.data_source == "csv"',

                                # Input: Select a file
                                fileInput("csv_file", "Choose CSV File",
                                          multiple = TRUE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")
                                ),

                                # Horizontal line
                                tags$hr(),

                                # Input: Checkbox if file has header
                                checkboxInput("header", "Header", TRUE),

                                # Input: Select separator
                                radioButtons("sep", "Separator",
                                             choices = c(Comma = ",",
                                                         Semicolon = ";",
                                                         Tab = "\t"),
                                             selected = ","),

                                # Input: Select quotes
                                radioButtons("quote", "Quote",
                                             choices = c(None = "",
                                                         "Double Quote" = '"',
                                                         "Single Quote" = "'"),
                                             selected = '"')

                              ),

                              conditionalPanel(
                                condition = 'input.data_source == "svc"',

                                # Input: Enter svc link
                                textInput(inputId = 'svc_text',
                                          label = 'SVC',
                                          placeholder = 'https://research.odk.path.org/#/projects/projectNumber/forms/projectName/submissions'),

                                # Input: Enter username
                                textInput(inputId = 'un',
                                          label = 'Username',
                                          placeholder = 'lucas.silbernagel@swisstph.ch'),

                                # Input: Enter password
                                passwordInput(inputId = 'pw',
                                              label = 'Password',
                                              placeholder = 'S3cur3_Password123'),

                                # Input: Enter timezone
                                textInput(inputId = 'tz',
                                          label = 'Timezone',
                                          placeholder = "Europe/Berlin"),
                              ),

                              # Horizontal line
                              tags$hr(),

                              actionButton("load_render_button", "Load and Preview Data")

                            ),

                            mainPanel(
                              # Output: Preview data file
                              dataTableOutput("contents")
                            )
                          )
                 ),

                 tabPanel('2. Select Visualisations',
                          sidebarLayout(
                            sidebarPanel(

                              # Input: Select data source
                              radioButtons(inputId = 'general_plots',
                                           label = 'Select general plots:',
                                           choices = c('Daily Submission Goal Donut' = 'donut',
                                                       'Submissions Over Time Line Chart (Cumulative)' = 'line_chart_cumsum',
                                                       'Submissions Over Time Line Chart (Non-Cumulative)' = 'line_chart_no_cumsum',
                                                       'Day of Week / Time of Day Heat Map' = 'day_heatmap',
                                                       'Calendar Heat Map' = 'cal_heatmap')
                              ),

                              # Horizontal line
                              tags$hr(),

                              radioButtons(inputId = 'question_plots',
                                           label = 'Select question-specific plots:',
                                           choices = c('Single Choice Question Pie Chart' = 'single_pie',
                                                       'Multiple Choice Question Bar Chart' = 'multiple_bar',
                                                       'Free Text Question Word Cloud' = 'wordcloud',
                                                       'Free Text Question Word Frequency Table' = 'freq_table')
                              ),

                            ),

                            mainPanel(
                              tabsetPanel(type = 'tab',
                                          tabPanel('Examples', tags$iframe(style = 'height:400px; width:100%; scrolling=yes', src = 'file:///C:/Users/silblu/Documents/GitHub/repvisforODK/explore/www/example_plots_repvis.pdf')))
                            )

                          )),

                 tabPanel('3. Set Parameters',
                          downloadButton("report", "Generate report"))
)

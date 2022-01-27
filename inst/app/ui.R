shiny::fluidPage(
  list(shiny::tags$head(shiny::HTML('<link rel="icon", href="https://lucidviews.github.io/gh-pages/logo_only.PNG",
                                 type="image/png" />'))),
  shiny::div(style="padding: 1px 0px; width: '100%'",
      shiny::titlePanel(
        title="", windowTitle="repvisforODK"
      )
  ),

# start navbar----------------------------------------------------------------------------------------------------------------------------------------
    shiny::navbarPage(includeCSS(system.file('visual', 'styles.css', package = 'repvisforODK')),
                          title = 'repvisforODK',
                          id = 'tab',
                          selected = '1. Select Data',

    # 'Select Data' tab-------------------------------------------------------------------------------------------------------------------------------
                          shiny::tabPanel('1. Select Data',
                            shiny::sidebarLayout(

            # Sidebar panel---------------------------------------------------------------------------------------------------------------------------
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
                                            value = 'GMT'),

                                # line break
                                shiny::tags$br(),

                                shiny::tags$h6('*required'),

                                # Horizontal line
                                shiny::tags$hr(),

                                # Input: button to trigger download of data from ODK Central
                                shiny::actionButton("load_preview_button", "Load and Preview Data"),

                                # when download of data was successful...
                                shiny::conditionalPanel(
                                  condition = 'output.data_flag == true',

                                  # Horizontal line
                                  shiny::tags$hr(),

                                  # Input: Filter data
                                  shiny::checkboxInput(inputId = 'filter_check',
                                                       label = 'Filter data by date'),

                                  # when check box for date filtering is checked...
                                  shiny::conditionalPanel(
                                    condition = 'input.filter_check == true',

                                    # Input: date col
                                    shiny::radioButtons(inputId = 'filter_col',
                                                        label = 'Date column to use for filtering',
                                                        choices = c('NA')),

                                    # Input: date range
                                    shiny::dateRangeInput('date_range',
                                                   label = 'Date range',
                                                   start = Sys.Date() - 2,
                                                   end = Sys.Date() + 2
                                    )
                                  ),

                                  # Horizontal line
                                  shiny::tags$hr(),

                                  # Input: button to jump to next tab
                                  shiny::actionButton('next1', 'Use Data and Next')
                                ),

                              ),

              # Main panel------------------------------------------------------------------------------------------------------------------------------
                              shiny::mainPanel(

                                shiny::conditionalPanel(
                                  condition = ("input.load_preview_button != 0"),

                                  # Data will be used as shown info
                                  shiny::icon('info-circle'),
                                  shiny::tags$h4('The data displayed below will be used for report generation - any filtering will be kept.')
                                  ),

                                # Output: Preview data file
                                DT::DTOutput("contents") %>% shinycssloaders::withSpinner(color = '#bf3227')
                              )
                            )
                   ),

    # 'Select Visualisations' tab----------------------------------------------------------------------------------------------------------------------
                   shiny::tabPanel('2. Select Visualisations',

                                   # warning message if data was not successfully loaded in previous step
                                   shiny::conditionalPanel(
                                     condition = 'output.data_flag != true',
                                     shiny::tags$h3('No data available from Step 1. Please go back and either upload a csv file or provide valid ODK Central credentials for an ODK form.')
                                     ),

                                    # if data was successfully loaded...
                                    shiny::conditionalPanel(
                                      condition = 'output.data_flag == true',
                                      shiny::sidebarLayout(
            # Sidebar panel----------------------------------------------------------------------------------------------------------------------------
                                        shiny::sidebarPanel(id = 'side2',

                                          # Input: Select general plots
                                          shiny::checkboxGroupInput(inputId = 'general_plots',
                                                      label = 'Select General Plots',
                                                      choices = c('Daily Submission Goal Donut' = 'donut',
                                                                  'Submissions Over Time Line Chart (Cumulative)' = 'line_chart_cumsum',
                                                                  'Submissions Over Time Line Chart (Non-Cumulative)' = 'line_chart_no_cumsum',
                                                                  'Day of Week / Time of Day Heat Map' = 'day_heatmap',
                                                                  'Calendar Heat Map' = 'cal_heatmap')
                                          ),

                                          # Horizontal line
                                          shiny::tags$hr(),

                                          # Input: Select question-specific plots
                                          shiny::checkboxGroupInput(inputId = 'question_plots',
                                                      label = 'Select Question-Specific Plots',
                                                      choices = c('Single Choice Question Pie Chart' = 'single_pie',
                                                                  'Multiple Choice Question Bar Chart' = 'multiple_bar',
                                                                  'Free Text Question Word Cloud' = 'wordcloud')
                                          ),

                                          # Horizontal line
                                          shiny::tags$hr(),

                                          # Input: button to jump to previous tab
                                          shiny::actionButton('prev1', 'Previous'),

                                          # only if at least one plot of either kind is selected...
                                          shiny::conditionalPanel(
                                            condition = 'input.question_plots.length > 0 || input.general_plots.length > 0',

                                            # Horizontal line
                                            shiny::tags$hr(),

                                            # Input: button to jump to next tab
                                            shiny::actionButton('next2', 'Next')
                                          )
                                        ),

              # Main panel---------------------------------------------------------------------------------------------------------------------------
                                        shiny::mainPanel(

                                          shiny::tags$h3('Visualisation Examples'),

                                          # one tab for each plot to show user an example image
                                          shiny::tabsetPanel(type = 'tab',
                                                      shiny::tabPanel('Daily Submission Goal Donut',
                                                                      shiny::tags$img(src = 'https://lucidviews.github.io/gh-pages/donut.PNG',
                                                                               style = 'height:auto; width:100%; max-width:900px;')
                                                                      ),

                                                      shiny::tabPanel('Submissions Over Time Line Chart (Cumulative)',
                                                                      shiny::tags$img(src = 'https://lucidviews.github.io/gh-pages/line_cumsum.PNG',
                                                                               style = 'height:auto; width:100%; max-width:900px;')
                                                      ),

                                                      shiny::tabPanel('Submissions Over Time Line Chart (Non-Cumulative)',
                                                                      shiny::tags$img(src = 'https://lucidviews.github.io/gh-pages/line_no_cumsum.PNG',
                                                                               style = 'height:auto; width:100%; max-width:900px;')
                                                      ),

                                                      shiny::tabPanel('Day of Week / Time of Day Heat Map',
                                                                      shiny::tags$img(src = 'https://lucidviews.github.io/gh-pages/weekday_heatmap.PNG',
                                                                               style = 'height:auto; width:100%; max-width:900px;')
                                                      ),

                                                      shiny::tabPanel('Calendar Heat Map',
                                                                      shiny::tags$img(src = 'https://lucidviews.github.io/gh-pages/cal_heatmap.PNG',
                                                                               style = 'height:auto; width:100%; max-width:900px;')
                                                      ),

                                                      shiny::tabPanel('Single Choice Question Pie Chart',
                                                                      shiny::tags$img(src = 'https://lucidviews.github.io/gh-pages/pie.PNG',
                                                                               style = 'height:auto; width:100%; max-width:900px;')
                                                      ),

                                                      shiny::tabPanel('Multiple Choice Question Bar Chart',
                                                                      shiny::tags$img(src = 'https://lucidviews.github.io/gh-pages/bar.PNG',
                                                                               style = 'height:auto; width:100%; max-width:900px;')
                                                      ),

                                                      shiny::tabPanel('Free Text Question Word Cloud',
                                                                      shiny::tags$img(src = 'https://lucidviews.github.io/gh-pages/wc.PNG',
                                                                               style = 'height:auto; width:100%; max-width:900px;')
                                                      )
                                                      )
                                          )
                                        )
                                      )
                             ),


    # 'Set Parameters' tab----------------------------------------------------------------------------------------------------------------------------
                     shiny::tabPanel('3. Set Parameters',

                                     # if no plots are selected in 2nd tab
                                     shiny::conditionalPanel(
                                       condition = 'input.question_plots.length == 0 && input.general_plots.length == 0',
                                       shiny::tags$h3('No visualisations selected in Step 2. Please select at least one visualisation.')
                                     ),

                                     shiny::conditionalPanel(
                                       condition = 'input.question_plots.length > 0 || input.general_plots.length > 0',

                                      shiny::fluidPage(
                                        shiny::fluidRow(

            # 1-6 page columns------------------------------------------------------------------------------------------------------------------------
                                          shiny::column(6,
                                                  # for plots
                                                  shiny::tags$h3('1. Plot Parameters'),

                                                  # if at least one general plot is selected
                                                  shiny::conditionalPanel(
                                                    condition = 'input.general_plots.length > 0 ',

                                                    shiny::tags$h5('For all selected General Plots'),

                                                    # Input: Enter date column link
                                                    shiny::selectInput(inputId = 'date_col_param',
                                                                     label = 'Timestamp column*',
                                                                     choices = c('NA')),

                                                    # Input: daily sub goal check
                                                    shiny::checkboxInput(inputId = 'sub_goal_check',
                                                                         label = 'Include daily submission goal in general plots'),

                                                    # if sub goal check box is checked
                                                    shiny::conditionalPanel(
                                                      condition = 'input.sub_goal_check == true',

                                                      # Input: daily sub goal
                                                      shiny::numericInput(inputId = 'sub_goal_param',
                                                                          label = 'Daily submission goal',
                                                                          value = 0)
                                                    ),

                                                    # line break
                                                    shiny::tags$br(),
                                                  ),

                                                  # if at least one question specific plot is selected and the lang flag (see server) is TRUE
                                                  shiny::conditionalPanel(
                                                    condition = 'input.question_plots.length > 0 && output.lang_flag == true',

                                                    shiny::tags$h5('For all selected Question-Specific Plots'),

                                                    # Input: Question label column
                                                    shiny::selectInput(inputId = 'label_col_param',
                                                                        label = 'Question label column to use for translation.',
                                                                        choices = c('NA')),


                                                    # Question choices column
                                                    shiny::selectInput(inputId = 'choice_col_param',
                                                                        label = 'Choice column to use for translation.',
                                                                        choices = c('NA')),

                                                    shiny::tags$br(),
                                                  ),

                                                  # if multiple choice question bar plots are selected
                                                  shiny::conditionalPanel(
                                                    condition = 'input.question_plots.indexOf("multiple_bar") > -1',

                                                    shiny::tags$h5('For Multiple Choice Question Bar Plots'),

                                                    # Input: delimiter check
                                                    shiny::checkboxInput(inputId = 'delimiter_check',
                                                                         label = 'Delimiter with which multiple choice question answers are separated is NOT a space (" ")'),

                                                    # if delimiter check box is checked
                                                    shiny::conditionalPanel(
                                                      condition = 'input.delimiter_check == true',

                                                      # Input: Enter delimiter
                                                      shiny::textInput(inputId = 'delimiter_param',
                                                                       label = 'Enter delimiter*',
                                                                       placeholder = 'e.g.: ,')
                                                    ),

                                                    # line break
                                                    shiny::tags$br(),
                                                  ),

                                                  # if wordcloud plots are selected
                                                  shiny::conditionalPanel(
                                                    condition = 'input.question_plots.indexOf("wordcloud") > -1',

                                                    shiny::tags$h5('For Wordclouds'),

                                                    # Input: Enter report language link
                                                      shiny::selectInput(inputId = 'text_col_param',
                                                                     label = 'Question(s) you want to generate a word cloud for*',
                                                                     choices = c('Specify question label column above first to get set of choices'),
                                                                     multiple = TRUE),

                                                    # line break
                                                    shiny::tags$br(),

                                                    # Input: Enter report language link
                                                    shiny::textInput(inputId = 'lang_wc_param',
                                                                     label = 'Language of answers to free text question(s)*',
                                                                     placeholder = 'e.g.: english'),

                                                    # line break
                                                    shiny::tags$br(),
                                                  ),

                                                  # if sub goal donut plots or line charts are selected
                                                  shiny::conditionalPanel(
                                                    condition = 'input.general_plots.indexOf("donut") > -1 || input.general_plots.indexOf("line_chart_cumsum") > -1 || input.general_plots.indexOf("line_chart_no_cumsum") > -1',

                                                    shiny::tags$h5('For Submission Goal Donut and/or Submissions Over Time Line Charts'),

                                                    # Input: exclude wday?
                                                    shiny::checkboxInput(inputId = 'exclude_wday_check',
                                                                         label = 'Select days of the week which will not be cosnidered in the data',
                                                                         value = FALSE),

                                                    # if exclude days of the week check box is checked
                                                    shiny::conditionalPanel(
                                                      condition = 'input.exclude_wday_check == true',

                                                      # Input: days of the week to exclude
                                                      shiny::selectInput(inputId = 'exclude_wday_param',
                                                                         label = 'Days of the week to exclude',
                                                                         choices = c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'),
                                                                         multiple = TRUE)
                                                    ),
                                                  ),

                                                  # line break
                                                  shiny::tags$br(),

                                                  shiny::tags$h6('*required'),

                                                  shiny::tags$h6('(only click "Generate Report" if all required parameters are specified)')
                                                  ),
            # 7-12 page columns------------------------------------------------------------------------------------------------------------------------
                                        shiny::column(6,
                                                  # for report
                                                  shiny::tags$h3('2. Report Parameters'),

                                                  # Input: Enter report language link
                                                  shiny::textInput(inputId = 'title_param',
                                                                   label = 'Please enter a title*',
                                                                   placeholder = 'e.g.: timci_report_Nov_21'),

                                                  # line break
                                                  shiny::tags$br(),

                                                  # Input: Enter report language link
                                                  shiny::textInput(inputId = 'author_param',
                                                                   label = 'Please enter your name (will be shown as author)*',
                                                                   placeholder = 'e.g.: Lucas Silbernagel')
                                                  )
                                         )
                                       ),

                                        # Horizontal line
                                        shiny::tags$hr(style = 'border-color: #337ab7;'),

                                        # Input: button to trigger report generation
                                        shiny::downloadButton("report_button", "Generate report"),

                                        # triggers pop up window to indicate that report generation has started (see server)
                                        shinyalert::useShinyalert(),

                                        # Horizontal line
                                        shiny::tags$hr(style = 'border-color: #337ab7;'),

                                        shiny::actionButton('prev2', 'Previous'),

                                       # line break
                                        shiny::tags$br()
                                       )
                                     )
                   ),
# footer----------------------------------------------------------------------------------------------------------------------------------------------
# general footer with logo and links to Swiss TPH website and GitHub repo (see footer.html file in html folder for source code)
shiny::div(class = "footer",
  shiny::includeHTML(system.file('visual', 'footer.html', package = 'repvisforODK'))
       )
    )

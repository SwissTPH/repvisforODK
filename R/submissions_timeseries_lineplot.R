#' Generating a line plot that shows the number of submissions over time
#'
#' The function shows a line plot with the number of submissions per day. It does account for days where no submissions were recorded by adding
#' the missing days wit a value of 0 so that also days were no data was collected are represented in the plot.
#' Optionally, a vertical line can be added that represents the daily number of submissions goal.
#' Further, weekends can be excluded since often there is no data collection happening and would therefore make the plot less concise and comprehensive.
#' Below the plot, there is a range slider which can be used to define the range of time that the plot shows. Depending on the overall time span of the
#' data, there are several buttons (1w = 1 week, 1m = 1 month, 6m = ..., 1y = ..., or all) that can be used to pre-define a time window that is applied to the range slider.
#' In order to determine the collection period the function \code{\link{collection_period}} is used.
#' Please note, that one and only one of the three data arguments (df, csv, svc) must be specified.
#'
#' @param df Data frame containing the ODK data that is to be used. Optional, defaults to NULL.
#' @param csv Character that specifies the path to the csv file that is to be read. Optional, defaults to NULL.
#' @param svc Logical that indicates whether the data shall be parsed using ruODK's \code{\link[ruODK]{odata_submission_get}}. Optional, defaults to FALSE.
#' @param date_col String that specifies the date or time stamp column in the data which is to be examined.
#' @param daily_submission_goal Integer or float that defines the number of daily submissions goal.
#' @param exclude_wday Integer (for one day) or integer vector (for multiple days) containing the day(s) of the week that shall not be included when generating the plot, defaults to NULL. Specify the days as following: 1 = Sun, 2 = Mon, ..., 7 = Sun.
#' @param cumulative Logical that determines whether the cumulative sum of submissions is used as values for y. Optional, defaults to TRUE.
#'
#' @return Plotly html-widget
#'
#' @export
#' @import dplyr plotly lubridate
#'
#' @examples
#' \dontrun{
#' # 1. with SVC
#' # ruODK needs to be set up for this function to work
#' repvisforODK::setup_ruODK(svc = 'example/svc.svc', un = 'exampleusername', pw = 'examplepassword', tz = 'Europe/Berlin', verbose = TRUE)
#'
#' submissions_timeseries_lineplot(svc = TRUE, daily_submission_goal = 4, date_col = 'start', exclude_wday = c(1, 7), cumulative = TRUE)
#'
#' # 2. with data frame
#' submissions_timeseries_lineplot(df = df_odk_data, daily_submission_goal = 4, date_col = 'start', exclude_wday = c(1, 7), cumulative = TRUE)
#'
#' # 3. with csv
#' submissions_timeseries_lineplot(csv = 'example/file/odk_data.csv', daily_submission_goal = 4, date_col = 'start', exclude_wday = c(1, 7), cumulative = TRUE)
#' }
submissions_timeseries_lineplot <- function(df = NULL, csv = NULL, svc = FALSE, date_col, daily_submission_goal = 0, exclude_wday = NULL, cumulative = TRUE) {

  # loading and manipulating data-------------------------------------------------------------------------------------------------------------------------------

  df <- repvisforODK::check_data_args(df, csv, svc)

  # stop if daily submission goal is negative
  if (daily_submission_goal < 0) {
    stop("The argument daily_submission_goal has to be defined as a positive integer or float.")
  }

  # get earliest and latest date in data
  date_limits = repvisforODK::collection_period(df = df, date_col = date_col)

  # give specified date column default name
  names(df)[names(df) == date_col] <- 'date'

  # get all dates of data collection period
  all_dates_in_period = seq.Date(date_limits[[1]], date_limits[[2]], 'days')

  # group df by date and summarize using count (note that here dates where no data was collected are missing)
  df_count = df %>% dplyr::mutate(date = as.Date(date)) %>% dplyr::count(date)

  # create df with all dates of the data collection period (containing ALL dates, also the ones where no data was collected)
  df_count_full <- data.frame(all_dates_in_period)
  # new count column with 0 if date is missing in df_count and with df_count$n if the date exists.
  df_count_full$n <- sapply(df_count_full$all_dates_in_period,
                              function(x) ifelse(x %in% df_count$date, df_count$n[df_count$date == x], 0),
                              USE.NAMES = F)

  # exclude certain days of the week if any were specified by the user
  if (!is.null(exclude_wday)) {
    df_count_full$wday <- lubridate::wday(df_count_full$all_dates_in_period, abbr = T)

    df_count_full <- df_count_full[!df_count_full$wday %in% exclude_wday, ]
  }

  # if user selects cumulative line chart, the cumulative sum of the count column is calculated
  if (cumulative) df_count_full <- df_count_full %>% dplyr::mutate(n = cumsum(n))

  # plotting----------------------------------------------------------------------------------------------------------------------------------------------------

  fig <- plotly::plot_ly(df_count_full, type = 'scatter', mode = 'lines', line = list(color = repvisforODK::set_color('red')), width = 900) %>%
    plotly::add_trace(
      x = ~all_dates_in_period,
      y = ~n,
      name = 'Submissions',
      showlegend = TRUE
      ) %>%
    plotly::layout(
           showlegend = TRUE,
           xaxis = list(title = 'Date',
                        # range slider to enable interactive time window selection
                        rangeslider = list(visible = T),
                        rangeselector = list(
                          # logic to display appropriate buttons with respect to the length of the data collection period
                          buttons =
                            if (nrow(df_count) < 63) {
                              list(
                                list(count=1, label="1w", step="week", stepmode="backward"),
                                list(step="all")
                              )
                            } else if (nrow(df_count) %in% 63:365) {
                              list(
                                list(count=1, label="1w", step="week", stepmode="backward"),
                                list(count=1, label="1m", step="month", stepmode="backward"),
                                list(step="all")
                              )
                            } else if (nrow(df_count) %in% 366:730) {
                              list(
                                list(count=1, label="1w", step="week", stepmode="backward"),
                                list(count=1, label="1m", step="month", stepmode="backward"),
                                list(count=6, label="6m", step="month", stepmode="backward"),
                                list(step="all")
                              )
                            } else {
                              list(
                                list(count=1, label="1w", step="week", stepmode="backward"),
                                list(count=1, label="1m", step="month", stepmode="backward"),
                                list(count=6, label="6m", step="month", stepmode="backward"),
                                list(count=1, label="1y", step="year", stepmode="backward"),
                                list(step="all")
                              )
                            }

                        ),
                        zerolinecolor = '#ffff',
                        zerolinewidth = 2,
                        gridcolor = 'ffff'),
           yaxis = list(zerolinecolor = '#ffff',
                        zerolinewidth = 2,
                        gridcolor = 'ffff',
                        title = 'Number of Submissions'),
           plot_bgcolor = '#e5ecf6',
           margin = 0.1
      )

  # if daily sub goal > 0, additional line that plots it
  if (daily_submission_goal > 0) {
    # with slope
    if (cumulative) {
      y = c(daily_submission_goal, daily_submission_goal * nrow(df_count_full))
      # constant
    } else {
      y = c(daily_submission_goal, daily_submission_goal)
    }

    # the actual line that shows number of submissions
    fig <- fig %>%
      plotly::add_trace(
        x = c(date_limits[[1]], date_limits[[2]]),
        y = y,
        name = 'Daily Submission Goal',
        showlegend = TRUE,
        line = list(color = repvisforODK::set_color('green'),
                    width = 2,
                    dash = 'dash')
      ) %>%
      plotly::layout(showlegend = TRUE)
  }

  # adding title to the html widget
  title = 'Number of Submissions per Day Over Time'
  fig <- repvisforODK::add_html_title_tag(fig, title)

  return(fig)
}

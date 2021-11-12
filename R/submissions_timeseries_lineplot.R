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
#' @param exclude_weekend Logical that determines whether weekends are excluded in the plot. Optional, defaults to TRUE.
#' @param cumulative Logical that determines whether the cumulative sum of submissions is used as values for y. Optional, defaults to TRUE.
#'
#' @return Plotly html-widget
#'
#' @export
#' @import dplyr plotly lubridate
#'
#' @examples
submissions_timeseries_lineplot <- function(df = NULL, csv = NULL, svc = FALSE, date_col, daily_submission_goal = 0, exclude_weekend = TRUE, cumulative = TRUE) {

  df <- repvisforODK::check_data_args(df, csv, svc)

  if (daily_submission_goal < 0) {
    stop("The argument daily_submission_goal has to be defined as a positive integer or float.")
  }

  date_limits = repvisforODK::collection_period(df = df, date_col = date_col)
  names(df)[names(df) == date_col] <- 'date'

  all_dates_in_period = seq.Date(date_limits[[1]], date_limits[[2]], 'days')
  df_count = df %>% dplyr::mutate(date = as.Date(date)) %>% dplyr::count(date)

  df_count_full <- data.frame(all_dates_in_period)
  df_count_full$n <- sapply(df_count_full$all_dates_in_period,
                              function(x) ifelse(x %in% df_count$date, df_count$n[df_count$date == x], 0),
                              USE.NAMES = F)
  if (cumulative) df_count_full <- df_count_full %>% dplyr::mutate(n = cumsum(n))

  if (exclude_weekend) {
    df_count_full$wday <- lubridate::wday(df_count_full$all_dates_in_period, abbr = T)

    df_count_full <- df_count_full[!df_count_full$wday %in% c(1, 7), ]
  }

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
                        rangeslider = list(visible = T),
                        rangeselector = list(
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

  if (daily_submission_goal > 0) {
    if (cumulative) {
      y = c(daily_submission_goal, daily_submission_goal * nrow(df_count_full))
    } else {
      y = c(daily_submission_goal, daily_submission_goal)
    }

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

#' Generating a line plot that shows the number of submissions over time
#'
#' The function shows a line plot with the number of submissions per day. It does account for days where no submissions were recorded by adding
#' the missing days wit a value of 0 so that also days were no data was collected are represented in the plot.
#' Optionally, a vertical line can be added that represents the daily number of submissions goal.
#' Further, weekends can be excluded since often there is no data collection happening and would therefore make the plot less concise and comprehensive.
#' Below the plot, there is a range slider which can be used to define the range of time that the plot shows. Depending on the overall time span of the
#' data, there are several buttons (1w = 1 week, 1m = 1 month, 6m = ..., 1y = ..., or all) that can be used to predefine a time window that is applied to the range slider.
#' In order to determine the collection period the function \code{\link{collection_period}} is used.
#'
#' @param df Data frame that contains the data which is to be examined.
#' @param with_goal Logical that determines whether a vertical line representing the number of daily submissions goal is plotted.
#' @param daily_submission_goal Integer or float that defines the number of daily submissions goal.
#' @param exclude_weekend Logical that determines whether weekends are excluded in the plot.
#'
#' @return Plotly html-widget
#' @export
#'
#' @examples
submissions_timeseries_lineplot <- function(df, with_goal = FALSE, daily_submission_goal = 0, exclude_weekend=TRUE) {

  if (with_goal && daily_submission_goal <= 0) {
    stop("The argument daily_submission_goal has to be defined as a positive integer or float which is not null it with_goal is set to TRUE.")
  } else if (with_goal == F && daily_submission_goal > 0) {
    stop('The argument with_goal is set to FALSE but there is a postive integer or float defined for daily_submission_goal.')
  }

  submission_date_col = colnames(df)[grepl('submission.*date', colnames(df), ignore.case = T)| grepl('date.*submission', colnames(df), ignore.case = T)]
  names(df)[names(df) == submission_date_col] <- 'submission_date'

  date_limits <- repvisforODK::collection_period(df)
  all_dates_in_period = seq.Date(date_limits[[1]], date_limits[[2]], 'days')
  df_count = df %>% mutate(submission_date = as.Date(submission_date)) %>% count(submission_date)
  df_count_full <- data.frame(all_dates_in_period)
  df_count_full$n <- sapply(df_count_full$all_dates_in_period,
                            function(x) ifelse(x %in% df_count$submission_date, df_count$n[df_count$submission_date == x], 0),
                            USE.NAMES = F)

  if (exclude_weekend) {
    df_count_full$wday <- lubridate::wday(df_count_full$all_dates_in_period, abbr = T)

    df_count_full <- df_count_full[!df_count_full$wday %in% c(1, 7), ]
  }

  fig <- plot_ly(df_count_full, type = 'scatter', mode = 'lines', width = 900) %>%
    add_trace(
      x = ~all_dates_in_period,
      y = ~n,
      name = 'Submissions',
      showlegend = TRUE
      ) %>%
    layout(
           title = 'Number of Submissions per Day Over Time',
           showlegend = TRUE,
           xaxis = list(rangeslider = list(visible = T),
                        rangeselector=list(
                          buttons=
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
           plot_bgcolor='#e5ecf6', margin = 0.1
      )

  if (with_goal) {
    fig <- fig %>%
      add_trace(
        x = c(date_limits[[1]], date_limits[[2]]),
        y = c(daily_submission_goal, daily_submission_goal),
        name = 'Daily Submission Goal',
        showlegend = TRUE,
        line = list(color = green,
                    width = 2,
                    dash = 'dash')
      ) %>%
      layout(showlegend = TRUE)
  }

  return(fig)
}

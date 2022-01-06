#' Generate a calendar heat map that shows the number of submissions for each day of the data collection period.
#'
#' The heat map can be created using any date or time stamp feature.
#' Please note, that one and only one of the three data arguments (df, csv, svc) must be specified.
#'
#' @param date_col String that specifies the date or time stamp column in the data which is to be examined.
#' @param daily_submission_goal Integer or float that defines the number of daily submissions goal.
#' @param df Data frame containing the ODK data that is to be used. Optional, defaults to NULL.
#' @param csv Character that specifies the path to the csv file that is to be read. Optional, defaults to NULL.
#' @param svc Logical that indicates whether the data shall be parsed using ruODK's \code{\link[ruODK]{odata_submission_get}}. Optional, defaults to FALSE.
#'
#' @return This function returns a ggplot object which contains a calendar heat map.
#'
#' @import ggplot2 plotly lubridate
#' @export
#'
#' @examples
#' \dontrun{
#' # 1. with SVC
#' # ruODK needs to be set up for this function to work
#' repvisforODK::setup_ruODK(svc = 'example/svc.svc', un = 'exampleusername', pw = 'examplepassword', tz = 'Europe/Berlin', verbose = TRUE)
#'
#' heatmap_calendar(svc = TRUE, daily_submission_goal = 4, date_col = 'start')
#'
#' # 2. with data frame
#' heatmap_calendar(df = df_odk_data, daily_submission_goal = 4, date_col = 'start')
#'
#' # 3. with csv
#' heatmap_calendar(csv = 'example/file/odk_data.csv', daily_submission_goal = 4, date_col = 'start')
#' }
heatmap_calendar <- function(df = NULL, csv = NULL, svc = FALSE, date_col, daily_submission_goal = 0){

  # stop if daily submission goal is negative
  if (daily_submission_goal < 0) {
    stop("The argument daily_submission_goal has to be defined as a positive integer or float.")
  }

  # loading and manipulating data-------------------------------------------------------------------------------------------------------------------------------

  # load data
  df <- repvisforODK::check_data_args(df, csv, svc)

  # give specified date column default name
  names(df)[names(df) == date_col] <- 'ndate'

  df1 <- df %>%
    mutate(ndate = as.Date(ndate)) %>%
    # group by date col and summarize using count
    dplyr::count(ndate)

  # finding min and max date of the collection period
  date_limits = repvisforODK::collection_period(df = df, date_col = 'ndate')

  # get all possible dates of the months of the earliest and the latest date in the data collection period
  day_seq <- data.frame(ndate = seq(lubridate::floor_date(date_limits[[1]], 'month'), as.Date(lubridate::ceiling_date(date_limits[[2]], "month") - 1), "days"))

  # merge data with all possible dates
  df2 <- merge(day_seq, df1, by = "ndate", all = TRUE)

  # assign 0 to all dates within data collection period that are NA
  df2$n[(df2$ndate >= date_limits[[1]]) & (df2$ndate <= date_limits[[2]]) & is.na(df2$n)] <- 0

  # get max number of submissions on one day
  max_n <- max(df2$n, na.rm = TRUE)

  df2 <- df2 %>%
    # isolate several time info out of the dates in the date col
    dplyr::mutate(weekday = lubridate::wday(ndate, label = TRUE, abbr = TRUE, week_start = 7, locale = 'English'),
                  month = lubridate::month(ndate, label = TRUE),
                  date = lubridate::yday(ndate),
                  week = lubridate::epiweek(ndate))

  df2$week[df2$month == "Dec" & df2$week == 1] = 53
  df2 <- df2 %>%
    dplyr::group_by(month) %>%
    dplyr::mutate(monthweek = 1 + week - min(week)) %>%
    dplyr::rename('Number of Submissions' = n)

  # plotting----------------------------------------------------------------------------------------------------------------------------------------------------

  ggp <- df2 %>%
    ggplot2::ggplot(aes(weekday,-week, fill = `Number of Submissions`)) +
    ggplot2::geom_tile(colour = "white")  +
    ggplot2::ylab(ifelse(daily_submission_goal > 0, paste0('* = Daily Submission Goal of ', daily_submission_goal,' reached'), '')) +
    # heat map specific theme
    ggplot2::theme(
          legend.key.width = unit(3, "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8, face = 'italic'),
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          legend.title.align = 0.5,
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 15),
          panel.border = element_rect(colour = "grey", fill=NA, size=1),
          panel.spacing = unit(3, 'lines'),
          plot.title = element_text(hjust = 0.5, size = 21, face = "bold",
                                    margin = margin(0,0,0.5,0, unit = "cm"))) +
    # facet wrap to create one heat map per month
    ggplot2::facet_wrap(~month, nrow = 6, ncol = 2, scales = "free")

  # with sub goal adjustments
  if (daily_submission_goal > 0) {
    ggp <- ggp +
      # signalize reach of sub goal with star on the day number + explanation on y-axis
      ggplot2::geom_text(aes(label = ifelse(`Number of Submissions` >= daily_submission_goal, paste0(lubridate::day(ndate), '*'), lubridate::day(ndate))), size = 3, color = "black") +
      # setting color bar settings in a way that all days below sub goal have red and all above have green grading
      ggplot2::scale_fill_gradientn(colours = repvisforODK::set_color('quadcolor'),
                                    name = "Number of submissions",
                                    guide = 'colourbar',
                                    values = c(0, daily_submission_goal/max_n-000.1, daily_submission_goal/max_n, 1))
    # with sub goal == 0 (default) adjustments
    } else {
    ggp <- ggp +
      # no star indicator
      ggplot2::geom_text(aes(label = lubridate::day(ndate)), size = 3, color = "black") +
      # default color bar
      ggplot2::scale_fill_gradientn(colours = repvisforODK::set_color('quadcolor'),
                                    name = "Number of submissions",
                                    guide = 'colourbar')
  }

  # convert ggplot object to plotly object
  ggp <- plotly::ggplotly(ggp, tooltip = c('fill'))

  ggp <- ggp %>%
    layout(legend = list(orientation = 'h'))

  # adding title to the html widget
  title = "Number of Submissions by Calendar Day"
  ggp <- repvisforODK::add_html_title_tag(ggp, title)

  return(ggp)
}

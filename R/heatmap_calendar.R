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

heatmap_calendar <- function(date_col, daily_submission_goal = 0, df = NULL, csv = NULL, svc = FALSE){

  if (daily_submission_goal < 0) {
    stop("The argument daily_submission_goal has to be defined as a positive integer or float.")
  }

  df <- repvisforODK::check_data_args(df, csv, svc)

  names(df)[names(df) == date_col] <- 'ndate'

  df1 <- df %>%
    mutate(ndate = as.Date(ndate)) %>%
    dplyr::count(ndate)

  sdate <- as.Date(min(df1$ndate))
  day_seq <- data.frame(ndate = seq(lubridate::floor_date(sdate, 'month'), as.Date(lubridate::ceiling_date(Sys.Date(), "month") - 1), "days"))
  df2 <- merge(day_seq, df1, by = "ndate", all = TRUE)
  df2$n[(df2$ndate >= sdate) & (df2$ndate <= Sys.Date()) & is.na(df2$n)] <- 0

  df2 <- df2 %>%
    dplyr::mutate(weekday = lubridate::wday(ndate, label = TRUE, abbr = TRUE, week_start = 7, locale = 'English'),
                  month = lubridate::month(ndate, label = TRUE),
                  date = lubridate::yday(ndate),
                  week = lubridate::epiweek(ndate))
  df2$week[df2$month == "Dec" & df2$week == 1] = 53
  df2 <- df2 %>%
    dplyr::group_by(month) %>%
    dplyr::mutate(monthweek = 1 + week - min(week)) %>%
    dplyr::rename('Number of Submissions' = n)

  ggp <- df2 %>%
    ggplot2::ggplot(aes(weekday,-week, fill = `Number of Submissions`)) +
    ggplot2::geom_tile(colour = "white")  +
    ggplot2::ylab(ifelse(daily_submission_goal > 0, '* = Daily Submission Goal reached', '')) +
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
    ggplot2::scale_fill_gradientn(colours = repvisforODK::set_color('tricolor'),
                         name = "Number of submissions",
                         guide = 'colourbar') +
    ggplot2::facet_wrap(~month, nrow = 6, ncol = 2, scales = "free")

  if (daily_submission_goal > 0) {
    ggp <- ggp + ggplot2::geom_text(aes(label = ifelse(`Number of Submissions` >= daily_submission_goal, paste0(lubridate::day(ndate), '*'), lubridate::day(ndate))), size = 3, color = "black")
  } else {
    ggp <- ggp + ggplot2::geom_text(aes(label = lubridate::day(ndate)), size = 3, color = "black")
  }

  ggp <- plotly::ggplotly(ggp, tooltip = c('fill'))

  ggp <- ggp %>%
    layout(legend = list(orientation = 'h'))

  return(ggp)
}

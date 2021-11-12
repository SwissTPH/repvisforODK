#' Generates a heat map of the number of submissions per weekday and hour.
#'
#' Takes any timestamp column in the data and aggregates by weekday and hour of the day. The generated plot then shows for each combination
#' the frequency. Please note, that one and only one of the three data arguments (df, csv, svc) must be specified.
#'
#' @param df Data frame containing the ODK data that is to be used. Optional, defaults to NULL.
#' @param csv Character that specifies the path to the csv file that is to be read. Optional, defaults to NULL.
#' @param svc Logical that indicates whether the data shall be parsed using ruODK's \code{\link[ruODK]{odata_submission_get}}. Optional, defaults to FALSE.
#' @param date_col String that specifies the date or time stamp column in the data which is to be examined.
#'
#' @return Plotly html-widget
#'
#' @import ggplot2 dplyr lubridate plotly
#' @export
#'
#' @examples
heatmap_wday_hourofday <- function(df = NULL, csv = NULL, svc = FALSE, date_col) {

  df <- repvisforODK::check_data_args(df, csv, svc)

  names(df)[names(df) == date_col] <- 'tstamp'

  df_wday_hour <- df %>%
    dplyr::mutate(wday = lubridate::wday(tstamp, label = TRUE, week_start = 1, abbr = TRUE, locale = 'English'),
                  hour = lubridate::hour(tstamp)) %>%
    dplyr::count(wday, hour, name="count_wday_hour") %>%
    dplyr::arrange(desc(wday))

  theme_heatmap <- ggplot2::theme_light() +
    ggplot2::theme(
          panel.grid = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=10),
          axis.text.y = element_text(size = 8),
          axis.text.x = element_text(size = 10)
          )

  ggp <- ggplot2::ggplot(df_wday_hour, aes(x = wday, y = hour, fill = count_wday_hour)) +
    ggplot2::geom_tile(colour="white") +
    ggplot2::scale_fill_gradientn(colours = repvisforODK::set_color('tricolor'),
                         name = "Number of submissions",
                         guide = 'colourbar') +
    ggplot2::scale_y_reverse(breaks=c(23:0), labels=c(23:0), expand = c(0,0)) +
    ggplot2::scale_x_discrete(expand = c(0,0)) +
    ggplot2::labs(y = "Hour of Day") +
    theme_heatmap

  ggp <- plotly::ggplotly(ggp, tooltip = c('fill'))

  ggp <- ggp %>%
    plotly::layout(xaxis = list(side = 'top'))

  # adding title to the html widget
  title = "Number of Submissions by Day of Week / Hour of Day"
  ggp <- repvisforODK::add_html_title_tag(ggp, title)

  return(ggp)
}

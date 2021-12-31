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
#' \dontrun{
#' # 1. with SVC
#' # ruODK needs to be set up for this function to work
#' repvisforODK::setup_ruODK(svc = 'example/svc.svc', un = 'exampleusername', pw = 'examplepassword', tz = 'Europe/Berlin', verbose = TRUE)
#'
#' heatmap_calendar(svc = TRUE, date_col = 'start')
#'
#' # 2. with data frame
#' heatmap_calendar(df = df_odk_data, date_col = 'start')
#'
#' # 3. with csv
#' heatmap_calendar(csv = 'example/file/odk_data.csv', date_col = 'start')
#' }
heatmap_wday_hourofday <- function(df = NULL, csv = NULL, svc = FALSE, date_col) {

  # loading and manipulating data-------------------------------------------------------------------------------------------------------------------------------

  # load data
  df <- repvisforODK::check_data_args(df, csv, svc)

  # give specified date column default name
  names(df)[names(df) == date_col] <- 'ndate'

  df_wday_hour <- df %>%
    # add new columns containing day of the week and hour of the day for each submission
    dplyr::mutate(wday = lubridate::wday(ndate, label = TRUE, week_start = 1, abbr = TRUE, locale = 'English'),
                  hour = lubridate::hour(ndate)) %>%
    # group by day of the week and hour of the day and summarize using count
    dplyr::count(wday, hour, name="Count") %>%
    dplyr::arrange(desc(wday))

  # plotting----------------------------------------------------------------------------------------------------------------------------------------------------

  # define theme for heat map
  theme_heatmap <- ggplot2::theme_light() +
    ggplot2::theme(
          panel.grid = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=10),
          axis.text.y = element_text(size = 8),
          )

  ggp <- ggplot2::ggplot(df_wday_hour, aes(x = wday, y = hour, fill = Count)) +
    ggplot2::geom_tile(colour="white") +
    # set fill colors
    ggplot2::scale_fill_gradientn(colours = repvisforODK::set_color('quadcolor'),
                         name = "Number of submissions",
                         guide = 'colourbar') +
    ggplot2::scale_y_reverse(breaks=c(23:0), labels=c(23:0), expand = c(0,0)) +
    ggplot2::scale_x_discrete(expand = c(0,0)) +
    ggplot2::labs(y = "Hour of Day") +
    theme_heatmap

  # convert ggplot object to plotly object
  ggp <- plotly::ggplotly(ggp, tooltip = c('fill'))

  ggp <- ggp %>%
    plotly::layout(xaxis = list(side = 'top'))

  # adding title to the html widget
  title = "Number of Submissions by Day of Week / Hour of Day"
  ggp <- repvisforODK::add_html_title_tag(ggp, title)

  return(ggp)
}

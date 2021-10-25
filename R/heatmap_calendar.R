#' Generate a calendar heat map that shows the number of submissions for each day of the data collection period.
#'
#' The heat map can be created using any date or time stamp feature.
#'
#' @param df Data frame that contains the data which is to be examined.
#' @param date_col String that specifies the date or time stamp column in the data which is to be examined.
#'
#' @return This function returns a ggplot object which contains a calendar heat map.
#'
#' @import ggplot2 plotly
#' @export
#'
#' @examples

heatmap_calendar <- function(df, date_col){

  names(df)[names(df) == date_col] <- 'ndate'

  df1 <- df %>%
    mutate(ndate = as.Date(ndate)) %>%
    dplyr::count(ndate)

  sdate <- as.Date(min(df1$ndate))
  day_seq <- data.frame(ndate = seq(lubridate::floor_date(sdate, 'month'), as.Date(lubridate::ceiling_date(Sys.Date(), "month") - 1), "days"))
  df2 <- merge(day_seq, df1, by = "ndate", all = TRUE)
  df2$n[(df2$ndate >= sdate) & (df2$ndate <= Sys.Date()) & is.na(df2$n)] <- 0

  df2 <- df2 %>%
    dplyr::mutate(weekday = lubridate::wday(ndate, label = FALSE, abbr = FALSE, week_start = 7),
                  month = lubridate::month(ndate, label = TRUE),
                  date = lubridate::yday(ndate),
                  week = lubridate::epiweek(ndate))
  df2$week[df2$month == "Dec" & df2$week == 1] = 53
  df2 <- df2 %>%
    dplyr::group_by(month) %>%
    dplyr::mutate(monthweek = 1 + week - min(week))

  ggp <- df2 %>%
    ggplot(aes(weekday,-week, fill = n)) +
    geom_tile(colour = "white")  +
    geom_text(aes(label = lubridate::day(ndate)), size = 2.5, color = "black") +
    theme(legend.position = "top",
          legend.key.width = unit(3, "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          legend.title.align = 0.5,
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 15),
          panel.border = element_rect(colour = "grey", fill=NA, size=1),
          plot.title = element_text(hjust = 0.5, size = 21, face = "bold",
                                    margin = margin(0,0,0.5,0, unit = "cm"))) +
    scale_fill_gradientn(colours = c("#ff0000", "#f9c800", "#a6d40d"),
                         #values = scales::rescale(c(-1, -0.05, 0, 0.05, 1)),
                         values = c(0, 5, 10),
                         name = "Number of facility submissions",
                         guide = guide_colorbar(title.position = "top",
                                                direction = "horizontal")) +
    facet_wrap(~month, nrow = 4, ncol = 3, scales = "free")

  ggp <- plotly::ggplotly(ggp)

  return(ggp)
}

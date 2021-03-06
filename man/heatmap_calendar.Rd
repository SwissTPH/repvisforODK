% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heatmap_calendar.R
\name{heatmap_calendar}
\alias{heatmap_calendar}
\title{Generate a calendar heat map that shows the number of submissions for each day of the data collection period.}
\usage{
heatmap_calendar(
  df = NULL,
  csv = NULL,
  svc = FALSE,
  date_col,
  daily_submission_goal = 0
)
}
\arguments{
\item{df}{Data frame containing the ODK data that is to be used. Optional, defaults to NULL.}

\item{csv}{Character that specifies the path to the csv file that is to be read. Optional, defaults to NULL.}

\item{svc}{Logical that indicates whether the data shall be parsed using ruODK's \code{\link[ruODK]{odata_submission_get}}. Optional, defaults to FALSE.}

\item{date_col}{String that specifies the date or time stamp column in the data which is to be examined.}

\item{daily_submission_goal}{Integer or float that defines the number of daily submissions goal.}
}
\value{
This function returns a ggplot object which contains a calendar heat map.
}
\description{
The heat map can be created using any date or time stamp feature.
Please note, that one and only one of the three data arguments (df, csv, svc) must be specified.
}
\examples{
\dontrun{
# 1. with SVC
# ruODK needs to be set up for this function to work
repvisforODK::setup_ruODK(svc = 'example/svc.svc', un = 'exampleusername', pw = 'examplepassword', tz = 'Europe/Berlin', verbose = TRUE)

heatmap_calendar(svc = TRUE, daily_submission_goal = 4, date_col = 'start')

# 2. with data frame
heatmap_calendar(df = df_odk_data, daily_submission_goal = 4, date_col = 'start')

# 3. with csv
heatmap_calendar(csv = 'example/file/odk_data.csv', daily_submission_goal = 4, date_col = 'start')
}
}

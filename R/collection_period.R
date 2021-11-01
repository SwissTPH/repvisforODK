#' Get the earliest and latest of your data
#'
#' The function finds the earliest and the latest submission dates in a data frame. It accounts for different spellings of the submission date column
#' by applying a case-insensitive RegEx formula that matches every combination of the words 'submission' and 'date'.
#' Please note, that one and only one of the three data arguments (df, csv, svc) must be specified.
#'
#' @param date_col String that specifies the date or time stamp column in the data which is to be examined.
#' @param df Data frame containing the ODK data that is to be used. Optional, defaults to NULL.
#' @param csv Character that specifies the path to the csv file that is to be read. Optional, defaults to NULL.
#' @param svc Logical that indicates whether the data shall be parsed using ruODK's \code{\link[ruODK]{odata_submission_get}}. Optional, defaults to FALSE.
#'
#' @return list
#'
#' @export
#'
#' @examples
collection_period <- function(date_col, df = NULL, csv = NULL, svc = FALSE){

  df <- repvisforODK::check_data_args(df, csv, svc)

  earliest_submission = as.Date(min(df[[date_col]]))
  latest_submission = as.Date(max(df[[date_col]]))

  return(list(earliest_submission, latest_submission))
}

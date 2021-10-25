#' Get the earliest and latest of your data
#'
#' The function finds the earliest and the latest submission dates in a data frame. It accounts for different spellings of the submission date column
#' by applying a case-insensitive RegEx formula that matches every combination of the words 'submission' and 'date'.
#'
#' @param df Data frame containing the ODK data that is to be used.
#' @param date_col String that specifies the date or time stamp column in the data which is to be examined.
#'
#' @return list
#'
#' @export
#'
#' @examples
collection_period <- function(df, date_col){
  earliest_submission = as.Date(min(df[[date_col]]))
  latest_submission = as.Date(max(df[[date_col]]))

  return(list(earliest_submission, latest_submission))
}

#' Get the earliest and latest of your data
#'
#' The function finds the earliest and the latest submission dates in a data frame. It accounts for different spellings of the submission date column
#' by applying a case-insensitive RegEx formula that matches every combination of the words 'submission' and 'date'.
#'
#' @param df Data frame containing the ODK data that is to be used.
#'
#' @return list
#' @export
#'
#' @examples
collection_period <- function(df){
  submission_date_col = colnames(df)[grepl('submission.*date', colnames(df), ignore.case = T)|grepl('date.*submission', colnames(df), ignore.case = T)]
  earliest_submission = as.Date(min(df[[submission_date_col]]))
  latest_submission = as.Date(max(df[[submission_date_col]]))

  return(list(earliest_submission, latest_submission))
}

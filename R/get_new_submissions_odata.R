#' Download all missing instances through ODATA
#'
#' Just like the \code{\link{get_new_submissions}} function, this function finds and downloads all submissions
#' which are already stored on ODK Central but not contained in the passed data. In contrast to its sister function, \code{\link{get_new_submissions_odata}}
#' does not create a delta of instance ID's between old and new data to find the missing instances but it identifies the latest
#' submission date in the  old data and uses it to create ODATA query that filters for submissions that were either submitted or updated or both
#' after this date. The new submissions can be returned by themselves or merged with the old data.
#'
#' @param csv Character that specifies the path to the csv file that is to be read. (Either csv or df must not null)
#' @param df Data frame that, specifies the data frame that is to be read. (Either csv or df must be null)
#' @param id_col Character that specifies the exact name of the instance ID column in the df/csv.
#' @param submission_date_col Character that specifies the exact name of the submission date column in the df/csv.
#' @param merge_data Boolean that specifies whether the new data shall be merged with the one that was given or not.
#' @param force_timezone If TRUE all time stamp values will be remain the same but their timezone will be changed to the one used in the old data. If FALSE time stamps will be converted in accordance with the timezone of the old data.
#'
#'
#'
#' @return Data frame
#'
#' @import ruODK glue plyr lubridate readr
#' @export
#'
#' @example
#' \dontrun{
#' }
get_new_submissions_odata <- function(csv=NULL, df=NULL, id_col, submission_date_col, merge_data=TRUE, force_timezone=TRUE){

  # checks whether ruODK is set up
  if (ruODK::ru_settings()[[2]]=='') {
    stop('Please run the function repvisforODK::setup_ruODK() with your credentials and svc of the form you want to examine.')
  }

  # loading old and new data-------------------------------------------------------------------------------------------------------------------------------

  # loading old data
  if (is.null(csv) & is.null(df)){
    stop('Please pass either a csv path or a data frame as an argument.')
  } else if(is.null(df) & !is.null(csv)){
    df = readr::read_csv(csv)
  }

  # finding the latest time stamp in the data and converting it to ODATA format
  if (class(df[[submission_date_col]]) == 'character' & grepl('T', df[[submission_date_col]][1])) {
    df[[submission_date_col]] <- sapply(df[[submission_date_col]],
                                        function(x) as.POSIXct(substring(gsub('T', ' ', x),
                                                                         1,
                                                                         nchar(x)-5)),
                                        USE.NAMES = FALSE)
    df[[submission_date_col]] <- as.POSIXct(df[[submission_date_col]], origin = "1970-01-01")
  } else if (!class(df[[submission_date_col]]) == c('POSIXct', 'POSIXt') | class(df[[submission_date_col]]) == 'date') {
    stop('The specified submission date column is not in the right format. Possible format are a) "2021-11-11T19:14:03.132Z" as class character or b) class POSIXt or POSIXct.')
  }

  print(class(df[[submission_date_col]][1]))
  print(df[[submission_date_col]][1])
  critical_tstamp = paste0(gsub(' ', 'T', as.character(max(df[[submission_date_col]]+1))),
                           'Z')

  # loading new data using ODATA filter to filter for all submissions after the critical time stamp
  new_data_df = ruODK::odata_submission_get(filter = paste0('__system/submissionDate gt ',
                                                            critical_tstamp,
                                                            ' or __system/updatedAt gt ',
                                                            critical_tstamp),
                                            download = FALSE)

  # data manipulation to prepare for merge-------------------------------------------------------------------------------------------------------------------------------

  # dropping ODATA download-specific columns that could let the code fail if non-ODATA-downloaded initial data (df) is used
  new_data_df = new_data_df[,!(names(new_data_df) %in% c('odata_context', 'system_edits'))]

  # if no new data is found
  if (nrow(new_data_df)==0) stop('Your data is up-to-date.')

  # conversion of important time stamp columns from string to their appropriate class in R
  for (col in c('start', 'end', 'system_submission_date')){
    new_data_df[col] = sapply(new_data_df[[col]],
                              function(x) strsplit(gsub('T', ' ', x),
                                                   split = '.',
                                                   fixed = T)[[1]][1],
                              USE.NAMES = F)

    new_data_df[col] = as.POSIXct(new_data_df[[col]],
                                  format='%Y-%m-%d %H:%M:%S')
  }

  # getting time zones of old and new data
  new_data_tz = attr(new_data_df[['today']][1],
                     'tzone')
  old_data_tz = attr(df[[submission_date_col]][1],
                     'tzone')

  # warning if time zones are not the matching
  if (new_data_tz != old_data_tz){
    warning(glue::glue('The timezones of new data ({new_data_tz}) and old data ({old_data_tz}) do not match. If force_timezone is set to TRUE all time stamp values will be remain the same but their timezone will be changed to the one used in the old data. If FALSE time stamps will be converted in accordance with the timezone of the old data. The current value of force_timezones is {force_timezone}.'))

    if (force_timezone){

      for (col in c('today', 'start', 'end', 'system_submission_date')) new_data_df[col] = lubridate::force_tz(new_data_df[col],
                                                                                                               tz=old_data_tz)

    } else{
      for (col in c('today', 'start', 'end', 'system_submission_date')) new_data_df[col] = lubridate::force_tz(new_data_df[col],
                                                                                                               tz=new_data_tz)
    }
  }


  new_data_df['today'] = as.Date(new_data_df[['today']])

  if (merge_data){

    # matching columns of old and new data based on levinstein distance
    df_levi_dist = as.data.frame(sapply(colnames(new_data_df),
                                        function(x) utils::adist(x, colnames(df))),
                                 row.names = colnames(df))

    matching_list = lapply(colnames(df_levi_dist),
                           function(x) c(x, rownames(df_levi_dist)[which(df_levi_dist[[x]] %in% min(df_levi_dist[[x]]))]))

    for (vec in matching_list){
      names(new_data_df)[names(new_data_df) == vec[1]] <- vec[2]
    }

    # merging data by column
    return(plyr::rbind.fill(df, new_data_df))
  }

  return(new_data_df)
}

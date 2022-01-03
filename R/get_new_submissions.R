#' Takes ODK data (data frame or csv) and downloads all missing submissions.
#'
#' -Disclaimer: this function takes significantly longer than its ODATA equivalent \code{\link{get_new_submissions_odata}}.
#' Only use it if the latter does not work with your use case.-
#' Potential reasons for use are:
#' a) Form on ODK is encrypted (ODATA is generally not supported for such forms)
#' b) \code{\link{get_new_submissions_odata}} only gets new submissions that were submitted after the most recent submission in the data (missing instances "in between" are not considered)
#'
#' This function uses the \code{\link{find_missing_instanceIDs}} function to find all submissions of an ODK form which are already stored on ODK Central but not loaded in your current data submissions and then downloads them.
#' The new submissions can either be appended to the old data or be returned separately. To do so, the function makes use of ruODK's \code{\link[ruODK]{submission_get}} function which
#' sends GET requests to ODK Centrals REST-API to retrieve data.
#'
#' @param csv Character that specifies the path to the csv file that is to be read. (Either csv or df must not null)
#' @param df Data frame that, specifies the data frame that is to be read. (Either csv or df must be null)
#' @param id_col Character that specifies the exact name of the instance ID in the df/csv.
#' @param merge_data Boolean that specifies whether the new data shall be merged with the one that was given or not.
#'
#' @return Data frame
#'
#' @import ruODK tibble
#' @export
#'
#' @examples
#' \dontrun{
#' # ruODK needs to be set up for this function to work
#' repvisforODK::setup_ruODK(svc = 'example/svc.svc', un = 'exampleusername', pw = 'examplepassword', tz = 'Europe/Berlin', verbose = TRUE)
#'
#' # load latest data
#' df <- ruODK::odata_submission_get()
#'
#' # call function on fraction of the data and only return the missing part
#' new_data_df <- get_new_submissions(df = df[10:nrow(df), ], id_col = 'id')
#' }
get_new_submissions <- function(csv=NULL, df=NULL, id_col, merge_data=TRUE){

  # checks whether ruODK is set up
  if (ruODK::ru_settings()[[2]]=='') {
    stop('Please run the function repvisforODK::setup_ruODK() with your credentials and svc of the form you want to examine.')
  }

  # loading old and new data-------------------------------------------------------------------------------------------------------------------------------

  # getting data and missing instances
  help_list = repvisforODK::find_missing_instanceIDs(csv, df, id_col)
  df_gni = help_list[[1]]
  missing_instances = help_list[[2]]

  # downloading all missing instances in JSON (nested list) format
  new_data_json = ruODK::submission_get(missing_instances)

  # data manipulation-------------------------------------------------------------------------------------------------------------------------------

  # converting nested list to data frame with cleaned names
  enframed_df = tibble::enframe(unlist(new_data_json))
  enframed_df$clean_name = sapply(enframed_df$name,
                                  function(x) gsub('.', '_', x, fixed = T),
                                  USE.NAMES = F)

  enframed_df$clean_name[enframed_df$clean_name=='meta_instanceID'] = id_col

  # creating new df with same format as new data
  new_data_df = data.frame(matrix(ncol=length(colnames(df_gni)),
                               nrow=sum(enframed_df$clean_name=='meta_instanceID')))

  # using column names of old data
  colnames(new_data_df) = colnames(df_gni)

  # loop variable to flag event
  c = 0

  #populating the new df with the new data cell by cell
  for (row in 1:nrow(enframed_df)){
    if (enframed_df$clean_name[row]=='today') c = c+1

    new_data_df[c, enframed_df$clean_name[row]] = enframed_df$value[row]
  }

  # converting start and end column to time stamps
  new_data_df$start = sapply(new_data_df$start,
                          function(x) strsplit(gsub('T', ' ', x), split = '.', fixed = T)[[1]][1],
                          USE.NAMES = F)
  new_data_df$start = as.POSIXct(new_data_df$start,
                              format='%Y-%m-%d %H:%M:%S')

  new_data_df$end = sapply(new_data_df$end,
                        function(x) strsplit(gsub('T', ' ', x), split = '.', fixed = T)[[1]][1],
                        USE.NAMES = F)
  new_data_df$end = as.POSIXct(new_data_df$end,
                            format='%Y-%m-%d %H:%M:%S')

  # data merge-------------------------------------------------------------------------------------------------------------------------------

  # either return merged data or only new data
  if (merge_data){

    # matching columns of old and new data based on levinstein distance
    df_levi_dist = as.data.frame(sapply(colnames(new_data_df),
                                        function(x) utils::adist(x, colnames(df_gni))),
                                 row.names = colnames(df_gni))

    matching_list = lapply(colnames(df_levi_dist),
                           function(x) c(x, rownames(df_levi_dist)[which(df_levi_dist[[x]] %in% min(df_levi_dist[[x]]))]))

    for (vec in matching_list){
      names(new_data_df)[names(new_data_df) == vec[1]] <- vec[2]
    }

    # merging data by column
    return(plyr::rbind.fill(df_gni, new_data_df))
  }

  return(new_data_df)
}



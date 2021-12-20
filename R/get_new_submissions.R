#' Download all missing submissions
#'
#' This function uses the \code{\link{find_missing_instanceIDs}} function to find all submissions of an ODK form which are already stored on ODK Central but not loaded in your current data submissions and then downloads them.
#' The new submissions can either be appended to the old data or be returned separately. To do so, the function makes use of ruODK's \code{\link[ruODK]{submission_get}} function which
#' sends GET requests to ODK Centrals REST-API to retrieve data.
#'
#' @param csv Character that specifies the path to the csv file that is to be read. (Either csv or df must not null)
#' @param svc Logical that indicates whether the data shall be parsed using ruODK's \code{\link[ruODK]{odata_submission_get}}. Optional, defaults to FALSE.
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
get_new_submissions <- function(svc = FALSE, csv=NULL, df=NULL, id_col, merge_data=TRUE){

  # getting data and missing instances
  help_list = repvisforODK::find_missing_instanceIDs(csv, df, svc, id_col)
  df_gni = help_list[[1]]
  missing_instances = help_list[[2]]

  # downloading all missing instances in JSON (nested list) format
  new_data_json = ruODK::submission_get(missing_instances)

  # converting nested list to data frame with cleaned names
  enframed_df = tibble::enframe(unlist(new_data_json))
  enframed_df$clean_name = sapply(enframed_df$name,
                                  function(x) gsub('.', '-', x, fixed = T),
                                  USE.NAMES = F)

  enframed_df$clean_name[enframed_df$clean_name=='meta-instanceID'] = id_col

  # creating new df with same format than new data
  new_data_df = data.frame(matrix(ncol=length(colnames(df_gni)),
                               nrow=sum(enframed_df$clean_name=='meta-instanceID')))
  # using column names of old data
  colnames(new_data_df) = colnames(df_gni)

  # checking that column names match
  for (col in unique(enframed_df$clean_name)){
    if (!col %in% colnames(df_gni)){
      stop(paste0('The column -',
                  col,
                  '- must exist in the data. Please consider renaming using -names(my_data)[names(my_data) == xy] <- xy-'))
    }
  }

  # loop variable to flag event
  c = 0

  #populating the new df with the new data cell by cell
  for (row in 1:nrow(enframed_df)){
    if (enframed_df$clean_name[row]=='today') c = c+1

    new_data_df[c, enframed_df$clean_name[row]] = enframed_df$value[row]
  }

  # converting start and end column to time stamp class
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

  # either return merged data or only new data
  ifelse(merge_data,
         return(rbind(df_gni, new_data_df)),
         return(new_data_df))
}



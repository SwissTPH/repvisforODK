#' Find all missing submissions
#'
#' This function finds all instance ID's of submissions of an ODK form which are already stored on ODK Central but not loaded in your current data.
#' It does so by downloading the most recent submission list from ODK Central for the respective form and compares the instance IDs with the ones
#' that are contained in the given data.
#'
#' @param id_col Character that specifies the exact name of the instance ID in the df/csv.
#' @param csv Character that specifies the path to the csv file that is to be read.
#' @param df Data frame that, specifies the data frame that is to be read.
#'
#' @return List
#' @export
#'
#' @examples
find_missing_instanceIDs <- function(id_col, csv=NULL, df=NULL){
  if (ruODK::ru_settings()[[2]]=='') stop('Please run the function repvisforODK::setup_ruODK() with your credentials and svc of the from you want to look at.')

  if (is.null(csv) & is.null(df)){
    stop('Please pass either a csv path or a data frame as an argument.')
  } else if(is.null(df) & !is.null(csv)){
    df_gni = readr::read_csv(csv)
  } else df_gni = df

  submissions_df = ruODK::submission_list()

  '%ni%' <- Negate('%in%')
  missing_instances = submissions_df$instance_id[submissions_df$instance_id %ni% df_gni[[id_col]]]
  if (length(missing_instances)==0) stop('There are no new instances.')

  return(list(df_gni, missing_instances))
}


#' Download all missing submissions
#'
#' This function uses the \code{\link{find_missing_instanceIDs}} function to find all submissions of an ODK form which are already stored on ODK Central but not loaded in your current data submissions and then downloads them.
#' The new submissions can either be appended to the old data or be returned separately. To do so, the function makes use of ruODK's \code{\link[ruODK]{submission_get}} function which
#' sends GET requests to ODK Centrals REST-API to retrieve data.
#'
#' @param id_col Character that specifies the exact name of the instance ID in the df/csv.
#' @param csv Character that specifies the path to the csv file that is to be read.
#' @param df Data frame that specifies the data frame that is to be read.
#' @param merge_data Boolean that specifies whether the new data shall be merged with the one that was given or not.
#'
#' @return Data frame
#' @export
#'
#' @examples
get_new_submissions <- function(id_col, csv=NULL, df=NULL, merge_data=TRUE){

  help_list = repvisforODK::find_missing_instanceIDs(id_col, csv, df)
  df_gni = help_list[[1]]
  missing_instances = help_list[[2]]

  new_data_json = ruODK::submission_get(missing_instances)

  enframed_df = tibble::enframe(unlist(new_data_json))
  enframed_df$clean_name = sapply(enframed_df$name,
                                  function(x) gsub('.', '-', x, fixed = T),
                                  USE.NAMES = F)

  enframed_df$clean_name[enframed_df$clean_name=='meta-instanceID'] = id_col

  new_data_df = data.frame(matrix(ncol=length(colnames(df_gni)),
                               nrow=sum(enframed_df$clean_name=='meta-instanceID')))
  colnames(new_data_df) = colnames(df_gni)

  for (col in unique(enframed_df$clean_name)){
    if (col %ni% colnames(df_gni)){
      stop(paste0('The column -',
                  col,
                  '- must exist in the data. Please consider renaming using -names(my_data)[names(my_data) == xy] <- xy-'))
    }
  }

  c = 0
  for (row in 1:nrow(enframed_df)){
    if (enframed_df$clean_name[row]=='today') c = c+1

    new_data_df[c, enframed_df$clean_name[row]] = enframed_df$value[row]
  }

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

  ifelse(merge_data,
         return(rbind(df_gni, new_data_df)),
         return(new_data_df))
}



#' Find all missing instances
#'
#' Find all instance ID's of an ODK form which are already stored on ODK Central but not loaded in your current data.
#'
#' @param id_col character, specifies the exact name of the instance ID in the df/csv.
#' @param csv character, specifies the path to the csv file that is to be read.
#' @param df data frame, specifies the data frame that is to be read.
#'
#' @return character vector
#' @export
#'
#' @examples
#' find_missing_instanceIDs('https://research.odk.path.org/v1/projects/4/forms/02-TIMCI-SPA-cgei.svc', 'meta-instanceID', df=df_test)
find_missing_instanceIDs <- function(id_col, csv=NULL, df=NULL){
  if (ru_settings()[[2]]=='') stop('Please run the function ODKlyse::setup_ruODK() with your credentials and svc of the from you want to look at.')

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


#' Download all missing instances
#'
#' Find and download all instances of an ODK form which are already stored on ODK Central but not loaded in your current data
#' and optionally append them to the data that was passed.
#'
#' @param id_col character, specifies the exact name of the instance ID in the df/csv.
#' @param csv character, specifies the path to the csv file that is to be read.
#' @param df data frame, specifies the data frame that is to be read.
#' @param merge_data logical, specifies whether the new data shall be merged with the one that was given or not.
#'
#' @return data frame
#' @export
#'
#' @examples
#' get_new_instances('meta-instanceID', df=df_senegal)
get_new_instances <- function(id_col, csv=NULL, df=NULL, merge_data=TRUE){

  help_list = ODKlyse::find_missing_instanceIDs(id_col, csv, df)
  df_gni = help_list[[1]]
  missing_instances = help_list[[2]]

  new_data_json = ruODK::submission_get(missing_instances)

  enframed_df = tibble::enframe(unlist(new_data_json))
  enframed_df$clean_name = sapply(enframed_df$name, function(x) gsub('.', '-', x, fixed = T), USE.NAMES = F)

  enframed_df$clean_name[enframed_df$clean_name=='meta-instanceID'] = id_col

  final_df = data.frame(matrix(ncol=length(colnames(df_gni)), nrow=sum(enframed_df$clean_name=='meta-instanceID')))
  colnames(final_df) = colnames(df_gni)

  for (col in unique(enframed_df$clean_name)){
    if (col %ni% colnames(df_gni)){
      stop(paste0('The column -', col, '- must exist in the data. Please consider renaming using -names(my_data)[names(my_data) == xy] <- xy-'))
    }
  }

  c = 0
  for (row in 1:nrow(enframed_df)){
    if (enframed_df$clean_name[row]=='today') c = c+1

    final_df[c, enframed_df$clean_name[row]] = enframed_df$value[row]
  }

  final_df$start = sapply(final_df$start, function(x) strsplit(gsub('T', ' ', x), split = '.', fixed = T)[[1]][1], USE.NAMES = F)
  final_df$start = as.POSIXct(final_df$start, format='%Y-%m-%d %H:%M:%S')

  final_df$end = sapply(final_df$end, function(x) strsplit(gsub('T', ' ', x), split = '.', fixed = T)[[1]][1], USE.NAMES = F)
  final_df$end = as.POSIXct(final_df$end, format='%Y-%m-%d %H:%M:%S')

  ifelse(merge_data, return(rbind(df_gni, final_df)), return(final_df))
}



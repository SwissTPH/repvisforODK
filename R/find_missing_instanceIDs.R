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

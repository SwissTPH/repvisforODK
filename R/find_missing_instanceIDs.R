#' Find all missing submissions
#'
#' This function finds all instance ID's of submissions of an ODK form which are already stored on ODK Central but not loaded in your current data.
#' It does so by downloading the most recent submission list from ODK Central for the respective form and compares the instance IDs with the ones
#' that are contained in the given data.
#'
#' @param csv Character that specifies the path to the csv file that is to be read. (Either csv or df must not null)
#' @param df Data frame that, specifies the data frame that is to be read. (Either csv or df must be null)
#' @param id_col Character that specifies the exact name of the instance ID in the df/csv.
#'
#' @return List
#'
#' @import ruODK
#' @export
#'
#' @examples
#' \dontrun{
#' # ruODK needs to be set up for this function to work
#' repvisforODK::setup_ruODK(svc = example/svc.svc, un = exampleusername, pw = examplepassword, tz = 'Europe/Berlin', verbose = TRUE)
#'
#' find_missing_instanceIDs(df = df_odk_data, id_col = 'instance_id')
#' }
find_missing_instanceIDs <- function(csv=NULL, df=NULL, id_col){

  # checks whether ruODK is set up
  if (ruODK::ru_settings()[[2]]=='') stop('Please run the function repvisforODK::setup_ruODK() with your credentials and svc of the from you want to look at.')

  # loading old data
  if (is.null(csv) & is.null(df)){
    stop('Please pass either a csv path or a data frame as an argument.')
  } else if(is.null(df) & !is.null(csv)){
    df = readr::read_csv(csv)
  }

  # getting lsit with all submissions incl. meta data
  submissions_df = ruODK::submission_list()

  # getting IDs of all instances that are missing
  missing_instances = submissions_df$instance_id[!submissions_df$instance_id %in% df[[id_col]]]

  # if no instances are missing...
  if (length(missing_instances)==0) stop('There are no new instances.')

  return(list(df, missing_instances))
}

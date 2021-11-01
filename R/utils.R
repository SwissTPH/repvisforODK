#' Connects to an ODK form.
#'
#' Establishes connection with ODK Central servers by passing log in credentials, timezone and svc to the
#' \code{\link[ruODK]{ru_setup}} function. By default it tries to retrieve this information from local system variables.
#'
#' @param svc Character
#' @param un Optional, character
#' @param pw Optional, character
#' @param tz Optional, character
#' @param verbose Optional, logical
#'
#' @return None
#'
#' @export
#' @import ruODK
#'
#' @examples
setup_ruODK <- function(svc, un=Sys.getenv('ODKC_UN'), pw=Sys.getenv('ODKC_PW'), tz=Sys.getenv('ODKC_TZ'), verbose=TRUE){
  ruODK::ru_setup(
    svc = svc,
    un = un,
    pw = pw,
    tz = tz,
    verbose = verbose
  )
}

# ----------------------------------------------------------------------------------------------------------------------

#' Checks whether one and only one of the three arguments that can be used to pass data was specified.
#'
#' To do so, the function checks whether the args are NULL which is also there default.
#' If more or less than are not NULL, the function throws an error.
#' If exactly one argument is specified, the functions reads the data it contains and returns it in a data frame.
#'
#' @param df Data frame that contains the data which is to be examined.
#' @param csv Character that specifies the path to the csv file that is to be read.
#' @param svc Logical that indicates whether the data shall be parsed using ruODK's \code{\link[ruODK]{odata_submission_get}}.
#'
#' @return Data frame
#'
#' @export
#' @import ruODK readr
#'
#' @examples
check_data_args <- function(df = NULL, csv = NULL, svc = FALSE) {

  if (sum(c(!is.null(df), !is.null(csv), svc)) != 1) {
    stop("Please use one and only one of the three arguments (df, csv, svc) to specify the data which is to be examined.")
  }

  if (!is.null(df)) {
    return(df)
  } else if (!is.null(csv)) {
    return(readr::read_csv(csv))
  } else if (svc) {
    if (ruODK::ru_settings()[[2]]=='') {
      stop('Please run the function repvisforODK::setup_ruODK() with your credentials and svc of the form you want to examine.')
    }
    return(ruODK::odata_submission_get(download = FALSE))
  }
}

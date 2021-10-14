#' Sets up ruODK
#'
#' Establishes connection with ODK Cnetral servers by passing log in credentials, timezone and svc to the
#' ruODK::ru_setup() function. By default it tries to retrieve this information from local system variables.
#'
#' @param svc character
#' @param un optional, character
#' @param pw optional, character
#' @param tz optional, character
#' @param verbose optional, logical
#'
#' @return none
#' @export
#'
#' @examples
#' setup_ruODK('https://research.odk.path.org/v1/projects/4/forms/test.svc')

setup_ruODK <- function(svc, un=Sys.getenv('ODKC_UN'), pw=Sys.getenv('ODKC_PW'), tz=Sys.getenv('ODKC_TZ'), verbose=TRUE){
  ruODK::ru_setup(
    svc = svc,
    un = un,
    pw = pw,
    tz = tz,
    verbose = verbose
  )
}

#' Connects R session to a form on ODK Central.
#'
#' Establishes connection with ODK Central servers by passing log in credentials, timezone and svc to the
#' \code{\link[ruODK]{ru_setup}} function. This is required in order to use most ruODK functions.
#' By default, it tries to retrieve this information from local system variables.
#'
#' @param svc Character that specifies the svc link which is used to identify an ODK form.
#' @param un Character that specifies the ODK Central username. Optional, defaults to Sys.getenv('ODKC_UN').
#' @param pw Character that specifies the ODK Central password. Optional, defaults to Sys.getenv('ODKC_PW').
#' @param tz Character that specifies the ODK Central tiem zone Optional, defaults to Sys.getenv('ODKC_TZ').
#' @param verbose Logical that sets the 'verbose' argument of the \code{\link[ruODK]{ru_setup}} function.
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

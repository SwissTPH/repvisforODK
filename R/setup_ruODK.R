#' Connects R session to a form on ODK Central.
#'
#' Establishes connection with ODK Central servers by passing log in credentials, timezone and svc to the
#' \code{\link[ruODK]{ru_setup}} function. This is required in order to use most ruODK functions.
#' By default, it tries to retrieve this information from local system variables.
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

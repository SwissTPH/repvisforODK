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

#------------------------------------------------------------------------------------------------------------------------

#' Outputs message in CLI with an info symbol in magenta color.
#'
#' @param msg String that is used as the message content.
#'
#' @return NULL
#'
#' @export
#' @import clisymbols crayon
#'
#' @examples
info_msg <- function(msg) {
  arrow <- clisymbols::symbol$info
  return(message(crayon::magenta(paste(arrow, msg))))
}

#-------------------------------------------------------------------------------------------------------------------------

#' Outputs a string containing one or vector containing multiple color codes.
#'
#' The function is supposed to help in keeping a concise color schema.
#'
#' @param val Character that specifies which color code is shall be returned.
#'
#' @return Character character vector
#'
#' @export
#'
#' @examples
set_color <- function(val) {
  if (val == 'green') {
    return('#1A9850')
  } else if (val == 'red') {
    return('#D73027')
  } else if (val == 'yellow') {
    return("#FDAE61")
  } else if (val == 'contrast_scale') {
    return(c("#D73027", "#A6D96A", "#A50026", "#FEE08B", "#66BD63", "#F46D43", "#1A9850", "#FDAE61", "#D9EF8B", "#006837"))
  } else if (val == 'continous_scale') {
    return(c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#D9EF8B", "#A6D96A", "#66BD63", "#1A9850", "#006837"))
  } else if (val == 'tricolor') {
    return(c('#D73027', "#FDAE61", '#1A9850'))
  }
}

#-------------------------------------------------------------------------------------------------------------------------

#' Splits a string after certain amount of characters, finds the last white space and inserts an html line break (<br>).
#'
#' @param string Character that contains the text which is to be split.
#' @param n Specifies the number of characters before which the last word is looked for.
#'
#' @return Character
#'
#' @export
#'
#' @examples
fit_title <- function(string, n) {

  sub <- substr(string, 1, n)
  final_string1 <- strsplit(sub, ' (?=[^ ]+$)', perl=TRUE)[[1]][1]
  final_string2 <- substr(string, nchar(final_string1) + 1, nchar(string))

  final_string_concat <- paste0('\n <b>', final_string1, ' <br>', final_string2, '</b>')

  return(final_string_concat)
}

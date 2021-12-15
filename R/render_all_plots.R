#' Renders a markdown file that contains all plots of the package and creates an html file.
#'
#' The rendered file is based on the all_plots.rmd template that comes with the package. It contains all plots that the package offers.
#' If you want to customize the markdown file before rendering it, you can access the template using \code{\link[rmarkdown]{draft}}.
#' For the latter, use the following arguments: file = 'INSERT_FILE_NAME', template = 'all_plots', package = 'repvisforODK, edit = TRUE'.
#'
#' @param output_file String that specifies the path where the generated file shall be stored.
#' @param title String that specifies the title parameter in the YAML header of the document.
#' @param date_col String that specifies the date or time stamp column in the data which is to be examined.
#' @param author String that defines the name that is put as the author in the YAML header of the markdown file. Optional, defaults to the system user name.
#' @param daily_submission_goal Integer or float that defines the number of daily submissions goal. Optional, defaults to 0.
#' @param exclude_wday Integer (for one day) or integer vector (for multiple days) containing the day(s) of the week that shall not be included when generating the plot, defaults to NULL. Specify the days as following: 1 = Sun, 2 = Mon, ..., 7 = Sun.
#' @param delimiter Character specifying the symbol that is used to separate multiple choices in the data. Optional, defaults to ' '.
#' @param lang_wc Character that specifies the language of the answers of the free text question. Check \code{\link[tm]{stopwords}} to find out more about stopwords list options.
#' @param lang Character that specifies the language of the answers of the free text question. Check \code{\link[tm]{stopwords}} to find out more about stopwords list options.
#' @param text_col Character or Character vector (if multiple questions shall be examined) that specifies the names of the columns of the free text questions.
#'
#' @return NULL
#'
#' @export
#' @import rmarkdown
#'
#' @examples
render_all_plots <- function(output_file,
                             title,
                             date_col,
                             lang_wc,
                             text_col,
                             author = Sys.info()['effective_user'][[1]],
                             daily_submission_goal = 0,
                             exclude_wday = NULL,
                             delimiter = ' ',
                             lang = 'english'
                             ) {

  file_path <- system.file('rmarkdown', 'all_plots_function.rmd', package = 'repvisforODK')

  rmarkdown::render(input = file_path,
                    output_file = output_file,
                    params = list(title = title,
                                  author = author,
                                  date_col = date_col,
                                  daily_submission_goal = as.character(daily_submission_goal),
                                  exclude_weekend = as.character(exclude_weekend),
                                  delimiter = delimiter,
                                  lang = lang,
                                  lang_wc = lang_wc,
                                  text_col = text_col
                                  )
  )
}

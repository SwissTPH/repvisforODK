#' Generates pie charts for single-choice questions.
#'
#' This function generally differentiates between parsing data directly from ODK using the svc option or passing data locally (df or csv).
#' If one provides an svc, the function automatically finds all questions that only received single choices as answers. Please note, that
#' questions which allow multiple choices but only have received single choices are not identified as such. This is because the extended from schema
#' which is provided by the ODK Central API and accessed using ruODK's \code{\link[ruODK]{form_schema_ext}} does not provide this info.
#' After identifying the questions, the functions also maps the question and choices labels according to their names so that the final plot
#' contains a meaningful title and legend. By default, the function assumes that there is no language differentiation implemented in th form.
#' If so, the argument lang_suffix has to be specified accordingly (see parameter description).
#' If svc is set to FALSE and the data is passed locally, the argument qvec has to be specified with the column names of single-choice questions
#' one wants to examine. Please note that for this case the column name will be displayed as the title while the row values will be displayed as
#' the choice labels.
#'
#'
#' @param svc Logical that indicates whether the data shall be parsed using ruODK's \code{\link[ruODK]{odata_submission_get}}. Optional, defaults to FALSE.
#' @param df Data frame containing the ODK data that is to be used. Optional, defaults to NULL.
#' @param csv Character that specifies the path to the csv file that is to be read. Optional, defaults to NULL.
#' @param qvec Character vector containing the column names of the single-choice questions that is to be examined.
#' @param lang_suffix Character vector containing the name and the country abbreviation of the language that is to be examined. E.g.: c('english', 'en').
#'
#' @return NULL
#'
#' @export
#' @import ruODK dplyr plyr plotly
#'
#' @examples
single_choice_question_pie <- function(svc = TRUE, df = NULL, csv = NULL, qvec = NULL, lang_suffix = NULL) {

  df <- repvisforODK::check_data_args(df, csv, svc)

  if (svc) {

    # deriving questions and choices from form schema
    choice_questions <- repvisforODK::identify_choice_questions(lang_suffix)
    df_schema <- choice_questions[[1]]
    qvec_pre <- choice_questions[[2]]

  } else if (is.null(qvec) & !svc) {
    stop('Please specify the qvec argument or run the function repvisforODK::setup_ruODK() with your credentials and svc of the form you want to examine.')
  } else {
    qvec_pre <- qvec
  }

  figs <- list()

  for (q in qvec_pre) {

    df_count <- plyr::count(df[[q]])
    df_count <- df_count[!is.na(df_count$x), ]

    # ensuring that only single choice questions are plotted
    if (class(df_count) == "data.frame") {
      if (!TRUE %in% grepl(' ', df_count$x)) {
        if (svc) {

          # mapping choice labels to their respective names
          df_count$label <- lapply(df_count$x,
                                   function(x) df_schema$choices_fin[df_schema$ruodk_name == q][[1]]$labels[match(x, df_schema$choices_fin[df_schema$ruodk_name == q][[1]]$values)])

          title = df_schema$labels_fin[df_schema$ruodk_name == q]

          fig <- plotly::plot_ly(data = df_count, labels = ~label, values = ~freq, type = 'pie', direction = 'clockwise',
                                 marker = list(colors = repvisforODK::set_color('contrast_scale')),
                                 hovertemplate = "%{label} <br>%{value}<extra></extra>")
          fig <- fig %>%
            plotly::layout(title = list(text = ifelse(nchar(title) < 40, title, ifelse(nchar(title) < 80, repvisforODK::fit_title(title, 40), repvisforODK::fit_title(repvisforODK::fit_title(title, 40), 85))),
                                        font = list(size = 10),
                                        y = 1,
                                        x = 0)
                           )
        } else {

          title = df_schema$labels_fin[df_schema$ruodk_name == q]

          fig <- plotly::plot_ly(data = df_count, labels = ~label, values = ~freq, type = 'pie', direction = 'clockwise',
                                 marker = list(colors = repvisforODK::set_color('contrast_scale')),
                                 hovertemplate = "%{label} <br>%{value}<extra></extra>")
          fig <- fig %>%
            plotly::layout(title = list(text = ifelse(nchar(title) < 40, title, ifelse(nchar(title) < 80, repvisforODK::fit_title(title, 40), repvisforODK::fit_title(repvisforODK::fit_title(title, 40), 85))),
                                        font = list(size = 10),
                                        y = 1,
                                        x = 0)
                           )
        }
        figs[[q]] <- plotly::plotly_build(fig)
      }
    }
  }
  return(figs)
}

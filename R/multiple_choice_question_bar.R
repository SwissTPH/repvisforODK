#' Generates a bar plot displaying the percentage of surveyees who selected a choice in a multiple choice questions out of all that answered the question.
#'
#' This function generally differentiates between parsing data directly from ODK using the svc option or passing data locally (df or csv).
#' If one provides an svc, the function automatically finds all questions that received multiple choices as answers. Please note, that
#' questions which allow multiple choices but only have received single choices are not identified as such and thus will not be displayed.
#' This is because the extended from schema which is provided by the ODK Central API and accessed using ruODK's \code{\link[ruODK]{form_schema_ext}} does not provide this info.
#' After identifying the questions, the functions also maps the question and choices labels according to their names so that the final plot
#' contains a meaningful title and legend. By default, the function assumes that there is no language differentiation implemented in the form.
#' If so, the argument lang_suffix has to be specified accordingly (see parameter description).
#' If svc is set to FALSE and the data is passed locally, the argument qvec has to be specified with the column names of single-choice questions
#' one wants to examine. Please note that for this case the column name will be displayed as the title while the row values will be displayed as
#' the choice labels.
#'
#' @param svc Logical that indicates whether the data shall be parsed using ruODK's \code{\link[ruODK]{odata_submission_get}}. Optional, defaults to FALSE.
#' @param df Data frame containing the ODK data that is to be used. Optional, defaults to NULL.
#' @param csv Character that specifies the path to the csv file that is to be read. Optional, defaults to NULL.
#' @param qvec Character vector containing the column names of the single-choice questions that is to be examined.
#' @param lang_suffix Character vector containing the name and the country abbreviation of the language that is to be examined. E.g.: c('english', 'en').
#' @param delimiter Character specifying the symbol that is used to separate multiple choices in the data. Optional, defaults to ' '.
#'
#' @return List
#'
#' @export
#' @import ruODK plotly
#'
#' @examples
multiple_choice_question_bar <- function(svc = TRUE, df = NULL, csv = NULL, qvec = NULL, lang_suffix = NULL, delimiter = ' ') {

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

    # ensuring that only single choice questions are plotted
    if (TRUE %in% grepl(delimiter, df[[q]])) {
      if (svc) {
        # mapping choice labels to their respective names and saving the result as a char vector in a new df col
        ansr <- lapply(df[[q]], function(x) strsplit(x, ' ', fixed = TRUE)[[1]])
        df$label <- I(lapply(ansr,
                             function(x) sapply(x,
                                                function(x) df_schema$choices_fin[df_schema$ruodk_name == q][[1]]$labels[match(x, df_schema$choices_fin[df_schema$ruodk_name == q][[1]]$values)],
                                                USE.NAMES = F)
                            )
                      )
      } else {
        df$label <- I(lapply(df[[q]], function(x) strsplit(x, delimiter, fixed = TRUE)[[1]]))
      }

      fin_vec <- c()
      for (vec in df$label) fin_vec <- c(fin_vec, vec)
      df_count_final <- as.data.frame(table(fin_vec))

      title = ifelse(svc, df_schema$labels_fin[df_schema$ruodk_name == q], q)

      num_peop_q <- nrow(df[!is.na(df[[q]]), ])

      fig <- plot_ly(data = df_count_final, x = ~fin_vec, y = ~Freq/num_peop_q,
                     type = 'bar',
                     text = ~Freq,
                     textposition = 'outside',
                     color = I('#A50026'),
                     hovertemplate = "%{label} <br>%{y}<extra></extra>")

      fig <- fig %>%
        layout(yaxis = list(tickformat = '.0%',
                            title = '% of Surveyees who Selected Choice*',
                            titlefont = list(size = 12),
                            range = c(0, 1)),
               xaxis = list(categoryorder = "total descending",
                            title = '*percentage only refers to surveyees who answered the question.',
                            titlefont = list(size = 10)),
               title = list(text = paste0(ifelse(nchar(title) < 120, paste0('<b>', title, '</b>'),
                                                 repvisforODK::fit_title(title, 80)), '<br><br><i>', num_peop_q, ' out of ', nrow(df), ' have answered this question.</i>'),
                            font = list(size = 12),
                            x = 0.8),
               plot_bgcolor = 'FCE0E0'
        )

      figs[[q]] <- plotly::plotly_build(fig)

    } else {
      if (!svc) {
        warning(paste0('The answers to question `', q, '` do not contain the specified delimiter `', delimiter, '`.'))
      }
      next
    }
  }
  return(figs)
}

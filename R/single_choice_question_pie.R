#' Generates pie charts for single-choice questions.
#'
#' This function generally differentiates between parsing data directly from ODK using the svc option or passing data locally (df or csv).
#' If one provides an svc, the function automatically finds all questions that only received single choices as answers. Please note, that
#' questions which allow multiple choices but only have received single choices are not identified as such. This is because the extended from schema
#' which is provided by the ODK Central API and accessed using ruODK's \code{\link[ruODK]{form_schema_ext}} does not provide this info.
#' After identifying the questions, the functions also maps the question and choices labels according to their names so that the final plot
#' contains a meaningful title and legend. By default, the function assumes that there is no language differentiation implemented in the form.
#' If so, the argument lang has to be specified accordingly (see parameter description).
#' If svc is set to FALSE and the data is passed locally, the argument qvec has to be specified with the column names of single-choice questions
#' one wants to examine. Please note that for this case the column name will be displayed as the title while the row values will be displayed as
#' the choice labels.
#'
#' @param svc Logical that indicates whether the data shall be parsed using ruODK's \code{\link[ruODK]{odata_submission_get}}. Optional, defaults to FALSE.
#' @param df Data frame containing the ODK data that is to be used. Optional, defaults to NULL.
#' @param csv Character that specifies the path to the csv file that is to be read. Optional, defaults to NULL.
#' @param qvec Character vector containing the column names of the single-choice questions that is to be examined, defaults to NULL.
#' @param lang Character containing the name of the language that is to be examined, defaults to NULL.
#' @param df_schema_ext Data frame that defines the schema of the from. Can be passed to the function to avoid downloading it multiple times. Optional, defaults to NULL.
#' @param choice_col String specifying the choices column that is to be changed, defaults to NULL.
#' @param label_col String specifying the labels column that is to be changed, defaults to NULL.
#'
#' @return List
#'
#' @export
#' @import ruODK plyr plotly
#'
#' @examples
#' \dontrun{
#' # 1. with SVC
#' # ruODK needs to be set up for this function to work
#' repvisforODK::setup_ruODK(svc = example/svc.svc, un = exampleusername, pw = examplepassword, tz = 'Europe/Berlin', verbose = TRUE)
#'
#' submissions_timeseries_lineplot(svc = TRUE, lang = 'english', choice_col = 'choices_english_(en)', label_col = 'label_english_(en)')
#'
#' # 2. with data frame and external form schema
#' df_schema = ruODK::form_schema_ext()
#'
#' submissions_timeseries_lineplot(df = df_odk_data, lang = 'english', df_schema_ext = df_schema, choice_col = 'choices_english_(en)', label_col = 'label_english_(en)')
#'
#' # 3. with csv and qvec
#' submissions_timeseries_lineplot(csv = 'example/file/odk_data.csv', lang = 'english', qvec = c('question1', 'question4'), choice_col = 'choices_english_(en)', label_col = 'label_english_(en)')
#' }
single_choice_question_pie <- function(svc = FALSE, df = NULL, csv = NULL, qvec = NULL, lang = NULL, df_schema_ext = NULL, choice_col = NULL, label_col = NULL) {

  # loading and manipulating data-------------------------------------------------------------------------------------------------------------------------------

  df <- repvisforODK::check_data_args(df, csv, svc)

  # stop if more than one source for chocie questions is defined
  if (sum(svc, !is.null(df_schema_ext), !is.null(qvec)) != 1) {

    stop('The function is not able to clearly identify single choice questions. Please only specify one out of svc, qvec and df_schema_ext. Note that if you have set svc to TRUE, you need to run the function setup_ruODK() with your credentials to log in to your ODK server.')

  } else if (!is.null(df_schema_ext) | svc) {

    # deriving questions and choices from form schema
    choice_questions <- repvisforODK::identify_choice_questions(lang = lang, df = df_schema_ext, choice_col = choice_col, label_col = label_col)
    df_schema <- choice_questions[[1]]
    qvec_pre <- choice_questions[[2]]

  } else {
    qvec_pre <- qvec
  }

  # empty list to store plots in
  figs <- list()

  for (q in qvec_pre) {

    df_count <- plyr::count(df[[q]])
    df_count <- df_count[!is.na(df_count$x), ]

    # ensuring that only single choice questions are plotted
    if (class(df_count) == "data.frame") {
      if (svc | !is.null(df_schema_ext)) {
        if (!TRUE %in% grepl(' ', df_count$x)) {

          # mapping choice labels to their respective names
          df_count$label <- lapply(df_count$x,
                                   function(x) df_schema$choices_fin[df_schema$ruodk_name == q][[1]]$labels[match(x, df_schema$choices_fin[df_schema$ruodk_name == q][[1]]$values)])

          # removing all HTML tags form the question label
          df_schema$labels_fin_clean <- lapply(df_schema$labels_fin, repvisforODK::remove_html_tags)
          title <- df_schema$labels_fin_clean[df_schema$ruodk_name == q]
          } else next

      } else {
        title <- q
        df_count$label <- df_count$x
      }

      # calculating number of people who answered the question
      num_peop_q <- nrow(df[!is.na(df[[q]]), ])

      # plotting----------------------------------------------------------------------------------------------------------------------------------------------------

      fig <- plotly::plot_ly(data = df_count, labels = ~label, values = ~freq, type = 'pie', direction = 'clockwise',
                             marker = list(colors = repvisforODK::set_color('contrast_scale')),
                             # hover content definition
                             hovertemplate = "%{label} <br>%{value}<extra></extra>")
      fig <- fig %>%
        plotly::layout(title = list(text = paste0('<br>' ,num_peop_q, ' out of ', nrow(df), ' have \nanswered this question.'),
                                    font = list(size = 12),
                                    y = 1,
                                    x = 0)
                       )

      # adding title to the html widget
      fig <- repvisforODK::add_html_title_tag(fig, title)

      # evaluate plotly objects before saving in list to avoid rendering bugs in the report
      figs[[q]] <- plotly::plotly_build(fig)
    }
  }
  return(figs)
}

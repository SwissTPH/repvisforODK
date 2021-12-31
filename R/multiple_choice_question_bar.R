#' Generates a bar plot displaying the percentage of surveyees who selected a choice in a multiple choice questions out of all that answered the question.
#'
#' This function generally differentiates between parsing data directly from ODK using the svc option or passing data locally (df or csv).
#' If one provides an svc, the function automatically finds all questions that received multiple choices as answers. Please note, that
#' questions which allow multiple choices but only have received single choices are not identified as such and thus will not be displayed.
#' This is because the extended from schema which is provided by the ODK Central API and accessed using ruODK's \code{\link[ruODK]{form_schema_ext}} does not provide this info.
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
#' @param qvec Character vector containing the column names of the single-choice questions that is to be examined, defaults to NULL. Only to be used when loading data through df or csv and when df_schema_ext is not used.
#' @param lang Character containing the name of the language that is to be examined, defaults to NULL.
#' @param delimiter Character specifying the symbol that is used to separate multiple choices in the data. Optional, defaults to ' '.
#' @param df_schema_ext Data frame that defines the schema of the from. Can be passed to the function to avoid downloading it multiple times. Optional, defaults to NULL.
#' @param choice_col String specifying the choices column that is to be changed, defaults to NULL.
#' @param label_col String specifying the labels column that is to be changed, defaults to NULL.
#'
#' @return List
#'
#' @export
#' @import ruODK plotly
#'
#' @examples
#' \dontrun{
#' # 1. with SVC
#' # ruODK needs to be set up for this function to work
#' repvisforODK::setup_ruODK(svc = 'example/svc.svc', un = 'exampleusername', pw = 'examplepassword', tz = 'Europe/Berlin', verbose = TRUE)
#'
#' submissions_timeseries_lineplot(svc = TRUE, lang = 'english', delimiter = ' ', choice_col = 'choices_english_(en)', label_col = 'label_english_(en)')
#'
#' # 2. with data frame and external form schema
#' df_schema = ruODK::form_schema_ext()
#'
#' submissions_timeseries_lineplot(df = df_odk_data, lang = 'english', df_schema_ext = df_schema, delimiter = ' ', choice_col = 'choices_english_(en)', label_col = 'label_english_(en)')
#'
#' # 3. with csv and qvec
#' submissions_timeseries_lineplot(csv = 'example/file/odk_data.csv', lang = 'english', qvec = c('question1', 'question4'), delimiter = ' ', choice_col = 'choices_english_(en)', label_col = 'label_english_(en)')
#' }
multiple_choice_question_bar <- function(svc = FALSE, df = NULL, csv = NULL, qvec = NULL, lang = NULL, df_schema_ext = NULL, delimiter = ' ', choice_col = NULL, label_col = NULL) {

  # loading and manipulating data-------------------------------------------------------------------------------------------------------------------------------

  df <- repvisforODK::check_data_args(df, csv, svc)

  # stop if more than one source for chocie questions is defined
  if (sum(svc, !is.null(df_schema_ext), !is.null(qvec)) != 1) {

    stop('The function is not able to clearly identify multiple choice questions. Please only specify one out of svc, qvec and df_schema_ext. Note that if you have set svc to TRUE, you need to run the function setup_ruODK() with your credentials to log in to your ODK server.')

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

  # counter for color alternation between each plot
  counter <- 0

  for (q in qvec_pre) {

    # ensuring that only single choice questions are plotted
    if (TRUE %in% grepl(delimiter, df[[q]])) {
      counter <- counter + 1

      if (svc | !is.null(df_schema_ext)) {

        # mapping choice labels to their respective names and saving the result as a char vector in a new df col
        ansr <- lapply(df[[q]], function(x) strsplit(x, delimiter, fixed = TRUE)[[1]])
        df$label <- I(lapply(ansr,
                             function(x) sapply(x,
                                                function(x) df_schema$choices_fin[df_schema$ruodk_name == q][[1]]$labels[match(x, df_schema$choices_fin[df_schema$ruodk_name == q][[1]]$values)],
                                                USE.NAMES = F)
                            )
                      )
      } else {
        df$label <- I(lapply(df[[q]], function(x) strsplit(x, delimiter, fixed = TRUE)[[1]]))
      }

      # separating single choice from multiple choice answers
      fin_vec_single <- c()
      fin_vec_multi <- c()
      for (vec in df$label){
        if (length(vec) > 1) {
          fin_vec_multi <- c(fin_vec_multi, vec)
        } else {
          fin_vec_single <- c(fin_vec_single, vec)
        }
      }

      # getting counts for both dfs
      df_s <- as.data.frame(table(fin_vec_single))
      colnames(df_s)[1] <- 'choice'

      df_m <- as.data.frame(table(fin_vec_multi))
      colnames(df_m)[1] <- 'choice'

      # outer join to merge both dfs without losing info
      df_merge <- merge(df_s, df_m, by = 'choice', all = TRUE)
      df_merge[is.na(df_merge)] <- 0
      df_merge$Freq.total <- df_merge$Freq.x + df_merge$Freq.y


      if(svc | !is.null(df_schema_ext)) {

        # removing all HTML tags form the question label
        df_schema$labels_fin_clean <- lapply(df_schema$labels_fin, repvisforODK::remove_html_tags)
        title <- df_schema$labels_fin_clean[df_schema$ruodk_name == q]

      } else title <- q

      num_peop_q <- nrow(df[!is.na(df[[q]]), ])

      # plotting----------------------------------------------------------------------------------------------------------------------------------------------------

      # base bar plot only showing the multiple choice proportion
      fig <- plot_ly(data = df_merge, x = ~choice, y = ~Freq.y/num_peop_q,
                     type = 'bar',
                     name = 'Among Multiple Choices',
                     text = ~Freq.total,
                     textposition = 'outside',
                     # alternating the colors between every plot for better contrast
                     color = I(ifelse(counter %% 2 == 0,
                                    repvisforODK::set_color('red'),
                                    repvisforODK::set_color('green')
                                    )),
                     # hover content definition
                     hovertemplate = "%{label} <br>%{y}<extra></extra>")

      # stacking second layer of bars on top for single choice proportion
      fig <- fig %>% add_trace(x = ~choice, y = ~Freq.x/num_peop_q,
                               type = 'bar',
                               name = 'As Single Choice',
                               marker = list(color = if (counter %% 2 == 0) "#FEE08B" else "#A6D96A")
                               )

      fig <- fig %>%
        layout(yaxis = list(tickformat = '.0%',
                            title = '% of Surveyees who Selected Choice*',
                            titlefont = list(size = 12),
                            range = c(0, 1)),
               xaxis = list(categoryorder = "total descending",
                            title = '*percentage only refers to surveyees who answered the question.',
                            titlefont = list(size = 10)),
               title = list(text = paste0(num_peop_q, ' out of ', nrow(df), ' have answered this question.'),
                            font = list(size = 12),
                            x = 0.8),
               plot_bgcolor = '#e5ecf6',
               barmode = 'stack'
        )

      # adding title to the html widget
      fig <- repvisforODK::add_html_title_tag(fig, title)

      # evaluate plotly objects before saving in list to avoid rendering bugs in the report
      figs[[q]] <- plotly::plotly_build(fig)

    } else {
      if (!svc | is.null(df_schema_ext)) {
        warning(paste0('The answers to question `', q, '` do not contain the specified delimiter `', delimiter, '`.'))
      }
      next
    }
  }

  return(figs)
}

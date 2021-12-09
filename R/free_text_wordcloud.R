#' Generates a wordcloud based on text from free text questions in an ODK form.
#'
#' The main computation, namely the creation of the corpus, the preprocessing of the text in the corpus
#' and the generation of the wordcloud is done through the function \code{\link{preprocess_wc_generation}}.
#' This function serves as a wrapper that applies the \code{\link{collection_period}} to every free text question
#' column that the user specified in the argument text_col.
#'
#' @param svc Logical that indicates whether the data shall be parsed using ruODK's \code{\link[ruODK]{odata_submission_get}}. Optional, defaults to FALSE.
#' @param df Data frame containing the ODK data that is to be used. Optional, defaults to NULL.
#' @param csv Character that specifies the path to the csv file that is to be read. Optional, defaults to NULL.
#' @param text_col Character or Character vector (if multiple questions shall be examined) that specifies the names of the columns of the free text questions.
#' @param lang_wc Character that specifies the language of the answers of the free text question. Check \code{\link[tm]{stopwords}} to find out more about stopwords list options.
#' @param lang Character that specifies the language of the answers of the free text question. Check \code{\link[tm]{stopwords}} to find out more about stopwords list options.
#' @param df_schema_ext Data frame that defines the schema of the from. Can be passed to the function to avoid downloading it multiple times. Optional, defaults to NULL.
#' @param choice_col String specifying the choices column that is to be changed, defaults to NULL.
#' @param label_col String specifying the labels column that is to be changed, defaults to NULL.
#'
#' @return List
#'
#' @export
#' @import tm wordcloud2
#'
#' @examples
free_text_wordcloud <- function(svc = FALSE, df = NULL, csv = NULL, text_col, lang_wc, lang = NULL, df_schema_ext = NULL, choice_col = NULL, label_col = NULL) {

  if (svc & !is.null(df_schema_ext)) {
    stop('Please specify one and only one of the arguments "svc" and "df_schema_ext".')
  }

  df <- repvisforODK::check_data_args(df, csv, svc)

  wc_list = lapply(text_col,
                   preprocess_wc_generation, df_c = df, lang_wc_c = lang_wc, svc_c = svc, lang_c = lang, df_schema_ext_c = df_schema_ext, choice_col_c = choice_col, label_col_c = label_col)

  return(wc_list)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#' Takes a data frame column, generates a corpus, preprocesses it and generates a word cloud.
#'
#' This function is used within \code{\link{free_text_wordcloud}}. It uses the tm package to create
#' a corpus, apply preprocessing and create a Term Document Matrix. Then wordcloud2 is used to create
#' the word cloud.
#'
#' @param text_col Character or Character vector (if multiple questions shall be examined) that specifies the names of the columns of the free text questions.
#' @param lang_wc_c Character that specifies the language of the answers of the free text question. Check \code{\link[tm]{stopwords}} to find out more about stopwords list options.
#' @param df_c Data frame containing the ODK data that is to be used. Optional, defaults to df which is the df parsed in \code{\link{free_text_wordcloud}}.
#' @param lang_c Character containing the name of the language that is to be examined.
#' @param svc_c Logical that specifies whether the data is coming directly from ODK or not.
#' @param df_schema_ext_c Data frame that defines the schema of the from. Can be passed to the function to avoid downloading it multiple times. Optional, defaults to NULL.
#' @param choice_col_c String specifying the choices column that is to be changed, defaults to NULL.
#' @param label_col_c String specifying the labels column that is to be changed, defaults to NULL.
#'
#' @return wordcloud2 html-widget
#'
#' @export
#' @import tm wordcloud2 DT
#'
#' @examples
preprocess_wc_generation <- function(text_col, lang_wc_c, df_c = df, lang_c = lang, svc_c = svc, df_schema_ext_c = df_schema_ext, choice_col_c = choice_col, label_col_c = label_col) {

  # isolating text in vector
  text = df_c[[text_col]]
  text = text[!is.na(text)]

  # creating corpus
  corpus <- tm::Corpus(tm::VectorSource(text))

  # preprocessing corpus
  corpus <- corpus %>%
    tm::tm_map(removeNumbers) %>%
    tm::tm_map(removePunctuation) %>%
    tm::tm_map(stripWhitespace)
  corpus <- tm::tm_map(corpus, content_transformer(tolower))
  corpus <- tm::tm_map(corpus, removeWords, stopwords(lang_wc_c))

  # creating TD matrix
  dtm <- tm::TermDocumentMatrix(corpus)
  dtm_matrix <- as.matrix(dtm)
  words <- sort(rowSums(dtm_matrix),decreasing = TRUE)
  df_wc <- data.frame(word = names(words),freq = words)

  # defining title
  if (svc_c | !is.null(df_schema_ext_c)) {

    if (svc_c) {
      df_schema <- ruODK::form_schema_ext()
    } else df_schema <- df_schema_ext_c

    df_schema = repvisforODK::rename_schema(df = df_schema, lang = lang_c, choice_col = choice_col_c, label_col = label_col_c)

    title = df_schema$labels_fin[df_schema$ruodk_name == text_col]
  } else title = text_col

  # generating word cloud
  wc = wordcloud2::wordcloud2(df_wc,
                              size=1.6,
                              color=repvisforODK::set_color('contrast_scale')
                              )

  # adding title to the html widget
  wc <- repvisforODK::add_html_title_tag(wc, title)

  return(wc)
}




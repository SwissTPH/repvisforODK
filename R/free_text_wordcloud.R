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
#' @param lang Character that specifies the language of the answers of the free text question. Check \code{\link[tm]{stopwords}} to find out more about stopwords list options.
#'
#' @return List
#'
#' @export
#'
#' @examples
free_text_wordcloud <- function(svc = TRUE, df = NULL, csv = NULL, text_col, lang = 'english') {

  df <- repvisforODK::check_data_args(df, csv, svc)

  wc_list = lapply(text_col, preprocess_wc_generation, df_pr = df)

  return(wc_list)
}

#' Takes a data frame column, generates a corpus, preprocesses it and generates a wordcloud.
#'
#' This function is used within \code{\link{free_text_wordcloud}}. It uses the tm package to create
#' a corpus, apply preprocessing and create a Term Document Matrix. Then wordcloud2 is used to create
#' the wordcloud.
#'
#' @param text_col Character or Character vector (if multiple questions shall be examined) that specifies the names of the columns of the free text questions.
#' @param df_pr Data frame containing the ODK data that is to be used. Optional, defaults to df which is the df parsed in \code{\link{free_text_wordcloud}}.
#' @param lang Character that specifies the language of the answers of the free text question. Check \code{\link[tm]{stopwords}} to find out more about stopwords list options.
#'
#' @return wordcloud2 html-widget
#'
#' @export
#' @import tm wordcloud2
#'
#' @examples
preprocess_wc_generation <- function(text_col, df_pr = df, lang = 'english') {

  # isolating text in vector
  text = df_pr[[text_col]]
  text = text[!is.na(text)]

  # creating corpus
  corpus <- tm::Corpus(tm::VectorSource(text))

  # preprocessing corpus
  corpus <- corpus %>%
    tm::tm_map(removeNumbers) %>%
    tm::tm_map(removePunctuation) %>%
    tm::tm_map(stripWhitespace)
  corpus <- tm::tm_map(corpus, content_transformer(tolower))
  corpus <- tm::tm_map(corpus, removeWords, stopwords(lang))

  # creating TD matrix
  dtm <- tm::TermDocumentMatrix(corpus)
  dtm_matrix <- as.matrix(dtm)
  words <- sort(rowSums(dtm_matrix),decreasing = TRUE)
  df_wc <- data.frame(word = names(words),freq = words)

  wc = wordcloud2::wordcloud2(df_wc, size=1.6, color=repvisforODK::set_color('contrast_scale'))

  return(wc)
}

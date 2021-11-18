#' Identifies all questions in a form that have choices.
#'
#' The function also accounts for different languages when the lang argument is specified.
#' In both cases the relevant question label and choice column is named the same to facilitate code progression.
#'
#' @param lang Character containing the name of the language that is to be examined, defaults to NULL.
#'
#' @return List
#'
#' @export
#' @import ruODK dplyr
#'
#' @examples
identify_choice_questions <- function(lang = NULL) {
  # deriving questions and choices from form schema
  repvisforODK::info_msg('Identifying single choice questions through extended form schema directly from ODK Central.')

  df_schema <- ruODK::form_schema_ext()
  df_schema <- df_schema %>%
    dplyr::filter(!grepl("generated_", name), type != 'structure')

  df_schema <- repvisforODK::rename_schema(df = df_schema, lang)

  # filtering for only questions that have choices and are not NA or NULL
  qvec_pre <- df_schema$ruodk_name[df_schema$choices_fin != 'NULL' & df_schema$choices_fin != 'NA']
  qvec_pre <- qvec_pre[!is.na(qvec_pre)]

  return(list(df_schema = df_schema, qvec_pre = qvec_pre))
}

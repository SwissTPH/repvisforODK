#' Identifies all questions in a form that have choices.
#'
#' The function also accounts for different languages when the lang_suffix argument is specified.
#' In both cases the relevant question label and choice column is named the same to facilitate code progression.
#'
#' @param lang_suffix Character vector containing the name and the country abbreviation of the language that is to be examined. E.g.: c('english', 'en').
#'
#' @return List
#'
#' @export
#' @import ruODK dplyr
#'
#' @examples
identify_choice_questions <- function(lang_suffix = NULL) {
  # deriving questions and choices from form schema
  repvisforODK::info_msg('Identifying single choice questions through extended form schema directly from ODK Central.')

  df_schema <- ruODK::form_schema_ext()
  df_schema <- df_schema %>%
    dplyr::filter(!grepl("generated_", name), type != 'structure')

  if (!is.null(lang_suffix)) {
    names(df_schema)[names(df_schema) == paste0('choices_', lang_suffix[1], '_(', lang_suffix[2], ')')] <- 'choices_fin'
    names(df_schema)[names(df_schema) == paste0('label_', lang_suffix[1], '_(', lang_suffix[2], ')')] <- 'labels_fin'
  } else {
    names(df_schema)[names(df_schema) == 'choices'] <- 'choices_fin'
    names(df_schema)[names(df_schema) == 'label'] <- 'labels_fin'
  }

  # filtering for only questions that have choices and are not NA or NULL
  qvec_pre <- df_schema$ruodk_name[df_schema$choices_fin != 'NULL' & df_schema$choices_fin != 'NA']
  qvec_pre <- qvec_pre[!is.na(qvec_pre)]

  return(list(df_schema = df_schema, qvec_pre = qvec_pre))
}

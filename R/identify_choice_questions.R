#' Identifies all questions in a form that have choices.
#'
#' The function also accounts for different languages when the lang argument is specified.
#' In both cases the relevant question label and choice column is named the same to facilitate code progression.
#'
#' @param lang Character containing the name of the language that is to be examined, defaults to NULL.
#' @param df_schema_ext Data frame that defines the schema of the from. Can be passed to the function to avoid downloading it multiple times. Optional, defaults to NULL.
#'
#' @return List
#'
#' @export
#' @import ruODK dplyr
#'
#' @examples

identify_choice_questions <- function(lang = NULL, df_schema_ext = NULL) {
  # deriving questions and choices from form schema
  repvisforODK::info_msg('Identifying single choice questions through extended form schema.')

  if (is.null(df_schema_ext)) {
    df_schema <- ruODK::form_schema_ext()
  } else {
    df_schema <- df_schema_ext
  }

  df_schema <- df_schema %>%
    dplyr::filter(!grepl("generated_", name), type != 'structure')

  df_schema <- repvisforODK::rename_schema(df = df_schema, lang)

  # filtering for only questions that have choices and are not NA or NULL
  qvec_pre <- df_schema$ruodk_name[df_schema$choices_fin != 'NULL' & df_schema$choices_fin != 'NA']
  qvec_pre <- qvec_pre[!is.na(qvec_pre)]

  return(list(df_schema = df_schema, qvec_pre = qvec_pre))
}

#-------------------------------------------------------------------------------------------------------------------------


#' Changes the name of the labels and choices column in an ODk form schema.
#'
#' This function to give the column used in another function a uniform name so that no matter what language (or no language at all) is chosen,
#' all functions still work as they refer to the unifrom name given by this function.
#'
#' @param df Data frame containing the ODK form schema
#' @param lang Language of the choice/label column which is to be changed. Optional, defaults to NULL.
#'
#' @return Data frame
#'
#' @export
#'
#' @examples
rename_schema <- function(df, lang = NULL) {

  lang_2_char <- substr(lang, 1, 2)

  if (!is.null(lang)) {
    names(df)[names(df) == paste0('choices_', lang, '_(', lang_2_char, ')')] <- 'choices_fin'
    names(df)[names(df) == paste0('label_', lang, '_(', lang_2_char, ')')] <- 'labels_fin'
  } else {
    names(df)[names(df) == 'choices'] <- 'choices_fin'
    names(df)[names(df) == 'label'] <- 'labels_fin'
  }

  return(df)
}

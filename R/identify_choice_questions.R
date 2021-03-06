#' Identifies all questions in a form that have choices.
#'
#' The function also accounts for different languages when the lang argument is specified.
#' In both cases the relevant question label and choice column is named the same to facilitate code progression.
#'
#' @param lang Character containing the name of the language that is to be examined, defaults to NULL.
#' @param df_schema_ext Data frame that defines the schema of the from. Can be passed to the function to avoid downloading it multiple times. Optional, defaults to NULL.
#' @param choice_col String specifying the choices column that is to be changed, defaults to NULL.
#' @param label_col String specifying the labels column that is to be changed, defaults to NULL.
#'
#' @return List
#'
#' @export
#' @import ruODK dplyr
#'
#' @examples
#' \dontrun{
#' # 1. Without df_schema_ext
#' choice_questions <- identify_choice_questions(lang = 'english', choice_col = 'choices_english_(en)', label_col = 'label_english_(en)')
#'
#' # 2. With df_schema_ext
#' # ruODK needs to be set up for this function to work
#' repvisforODK::setup_ruODK(svc = 'example/svc.svc', un = 'exampleusername', pw = 'examplepassword', tz = 'Europe/Berlin', verbose = TRUE)
#'
#' df_schema <- ruODK::form_schema_ext()
#'
#' choice_questions <- identify_choice_questions(lang = 'english', df_schema_ext = df_schema, choice_col = 'choices_english_(en)', label_col = 'label_english_(en)')
#' }
identify_choice_questions <- function(lang = NULL, df_schema_ext = NULL, choice_col = NULL, label_col = NULL) {
  # deriving questions and choices from form schema
  repvisforODK::info_msg('Identifying single choice questions through extended form schema.')

  if (is.null(df_schema_ext)) {

    # checks whether ruODK is set up
    if (ruODK::ru_settings()[[2]]=='') {
      stop('Please run the function repvisforODK::setup_ruODK() with your credentials and svc of the form you want to examine or pass a from schema using df_schema_ext.')
    }

    df_schema <- ruODK::form_schema_ext()
  } else {
    df_schema <- df_schema_ext
  }

  df_schema <- df_schema %>%
    dplyr::filter(!grepl("generated_", name), type != 'structure')

  df_schema <- repvisforODK::rename_schema(df = df_schema, lang = lang, choice_col = choice_col, label_col = label_col)

  # filtering for only questions that have choices and are not NA or NULL
  qvec_pre <- df_schema$ruodk_name[df_schema$choices_fin != 'NULL' & df_schema$choices_fin != 'NA']
  qvec_pre <- qvec_pre[!is.na(qvec_pre)]

  return(list(df_schema = df_schema, qvec_pre = qvec_pre))
}

#-------------------------------------------------------------------------------------------------------------------------

#' Changes the name of the labels and choices column in an ODK form schema.
#'
#' This function to give the column used in another function a uniform name so that no matter what language (or no language at all) is chosen,
#' all functions still work as they refer to the uniform name given by this function.
#'
#' @param df Data frame containing the ODK form schema
#' @param lang String specifying the language of the choice/label column which is to be changed. Optional, defaults to NULL.
#' @param choice_col String specifying the choices column that is to be changed, defaults to NULL.
#' @param label_col String specifying the labels column that is to be changed, defaults to NULL.
#'
#' @return Data frame
#'
#' @export
#'
#' @examples
rename_schema <- function(df, lang = NULL, choice_col = NULL, label_col = NULL) {

  lang_2_char <- substr(lang, 1, 2)

  if (!is.null(lang) & is.null(choice_col) & is.null(label_col)) {
    names(df)[names(df) == paste0('choices_', lang, '_(', lang_2_char, ')')] <- 'choices_fin'
    names(df)[names(df) == paste0('label_', lang, '_(', lang_2_char, ')')] <- 'labels_fin'
  } else if (is.null(lang) & is.null(choice_col) & is.null(label_col)) {
    names(df)[names(df) == 'choices'] <- 'choices_fin'
    names(df)[names(df) == 'label'] <- 'labels_fin'
  } else if (is.null(lang) & !is.null(choice_col) & !is.null(label_col)) {
    names(df)[names(df) == choice_col] <- 'choices_fin'
    names(df)[names(df) == label_col] <- 'labels_fin'
  } else {
    stop('Please specify either choice_col and label_col OR lang OR none of the arguments mentioned. At the moment there is a conflict between these arguments.')
  }

  return(df)
}

#' Get the columns corresponding to a `select multiple` question
#'
#' @param .df The survey data frame
#' @param question_label The label of the question
#' @param group_sep The group separator used in the KoBo export
#'
#' @return A list of columns that correspond to each individual possible choice
#' for `question_label`
#' @export
#'
#' @examples
#' get_question_columns(lby_msna2021, "safety_concerns")
#' get_question_columns(lby_msna2021, "safety_concerns_boys")
#' get_question_columns(lby_msna2021, "safety_concerns_girls")
get_question_columns <- function(.df, question_label, group_sep = "/") {
  question_regex <- paste0("^", question_label, group_sep, "[A-Za-z_]+$")
  return(
    .df %>%
      dplyr::select(dplyr::matches(question_regex)) %>%
      colnames()
  )
}

#' Parse a KoBo expression into R.
#'
#' @param statement a string representing a KoBo expression
#' @param as_string whether to transform the result of parsing into an expression
#'
#' @return an R expression or its string representation
#' @export
#'
#' @examples
#' kobo_parse_expression("${hoh}")
#' kobo_parse_expression("${hoh}", as_string = TRUE)
#' kobo_parse_expression("selected(${hoh}, 'no')")
#' kobo_parse_expression("selected(${hoh}, 'no')", as_string = TRUE)
#' lby_msna %>%
#'   dplyr::filter(!!kobo_parse_expression("selected(${hoh}, 'no')")) %>%
#'   dplyr::distinct(hoh)
kobo_parse_expression <- function(statement, as_string = FALSE) {
  variable_name_regex <- r"(\$\{([a-zA-Z_0-9]+)\})"
  and_regex <- r"((?<=[\s*|\}])and(?=[\s*|\$]))"
  or_regex <- r"((?<=[\s*|\}])or(?=[\s*|\$]))"

  not_regex <- "not\\s*(?=\\()"
  selected_regex <- "selected\\(\\s*(\\$\\{[a-zA-Z_0-9]+\\})\\s*,\\s*'([a-zA-Z_0-9]+)'\\)"
  # The order of replacements matters. The definition of the selected_regex
  # depends of matching a variable before having it replaced to match
  # dplyr's syntax
  parsed_statement <- statement %>%
    stringr::str_squish() %>%
    stringr::str_replace_all("\"", "'") %>%
    stringr::str_replace_all(selected_regex, " \\1 == \"\\2\" ") %>%
    stringr::str_replace_all(and_regex, " & ") %>%
    stringr::str_replace_all(or_regex, " | ") %>%
    stringr::str_replace_all(not_regex, " !") %>%
    stringr::str_replace_all(variable_name_regex, " .data[[\"\\1\"]] ") %>%
    stringr::str_squish() %>%
    styler::style_text(strict = TRUE) %>%
    as.character()

  if (as_string) {
    return(parsed_statement)
  }
  return(rlang::parse_expr(parsed_statement))
}

#' @importFrom magrittr %>%
NULL

kobo_parse_relevant <- function(statement, as_string = FALSE) {
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

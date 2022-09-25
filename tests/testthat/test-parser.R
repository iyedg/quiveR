test_cases <- list(
  "variable names" = "${hoh}",
  "selected statements" = "selected(${phone_challenges},  'other')",
  "selected statements inside another function" = 'not(selected(${health_issues_child},"none"))',
  "or operator" = "selected(${phone_type},'feature_phone') or selected(${phone_type},'smartphone')",
  "and operator" = "selected(${phone_type},'feature_phone') and selected(${phone_type},'smartphone')",
  "operator without spaces" = "${enrolled_boys_6_14}<${male_children}or${enrolled_girls_6_14}<${female_children}"
)

test_that("variable names", {
  statement <- test_cases["variable names"]
  expect_equal(
    kobo_parse_relevant(statement, as_string = TRUE),
    ".data[[\"hoh\"]]"
  )
  expect_error(
    kobo_parse_relevant(statement, as_string = FALSE),
    NA
  )
})

test_that("selected statements", {
  statement <- test_cases["selected statements"]
  expect_equal(
    kobo_parse_relevant(statement, as_string = TRUE),
    ".data[[\"phone_challenges\"]] == \"other\""
  )
  expect_error(
    kobo_parse_relevant(statement, as_string = FALSE),
    NA
  )

  statement <- test_cases["selected statements inside another function"]
  # NOTE: This is not a great case as it requires the `not` function to work as well
  expect_equal(
    kobo_parse_relevant(statement, as_string = TRUE),
    "!(.data[[\"health_issues_child\"]] == \"none\")"
  )
  expect_error(
    kobo_parse_relevant(statement, as_string = FALSE),
    NA
  )
})
test_that("and operator", {
  statement <- test_cases["and operator"]
  expect_equal(
    kobo_parse_relevant(statement, as_string = TRUE),
    ".data[[\"phone_type\"]] == \"feature_phone\" & .data[[\"phone_type\"]] == \"smartphone\""
  )
  expect_error(
    kobo_parse_relevant(statement, as_string = FALSE),
    NA
  )
})

test_that("or operator", {
  statement <- test_cases["or operator"]
  expect_equal(
    kobo_parse_relevant(statement, as_string = TRUE),
    ".data[[\"phone_type\"]] == \"feature_phone\" | .data[[\"phone_type\"]] == \"smartphone\""
  )
  expect_error(
    kobo_parse_relevant(statement, as_string = FALSE),
    NA
  )
})

test_that("operator without spaces", {
  statement <- test_cases["operator without spaces"]
  expect_equal(
    kobo_parse_relevant(statement, as_string = TRUE),
    ".data[[\"enrolled_boys_6_14\"]] < .data[[\"male_children\"]] | .data[[\"enrolled_girls_6_14\"]] < .data[[\"female_children\"]]" # nolint
  )
  expect_error(
    kobo_parse_relevant(statement, as_string = FALSE),
    NA
  )
})

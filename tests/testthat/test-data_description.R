test_that("groups of questions identification works", {
  question_label <- "safety_concerns"
  output <- lby_msna2021 %>%
    get_question_columns(question_label = question_label)

  expected_columns <- lby_msna2021_choices %>%
    dplyr::filter(.data[["list_name"]] == "pr_4_safety_concerns_list") %>%
    dplyr::pull(name) %>%
    sapply(., FUN = function(x) {
      paste0(question_label, "/", x)
    }) %>%
    as.vector()

  expect_true(
    identical(
      output, expected_columns
    )
  )

  question_label <- "safety_concerns_boys"
  output <- lby_msna2021 %>%
    get_question_columns(question_label = question_label, group_sep = "\\/")

  expected_columns <- lby_msna2021_choices %>%
    dplyr::filter(.data[["list_name"]] == "pr_8_safety_concerns_boys_list") %>%
    dplyr::pull(name) %>%
    sapply(., FUN = function(x) {
      paste0(question_label, "/", x)
    }) %>%
    as.vector()

  expect_true(
    identical(
      output, expected_columns
    )
  )


  question_label <- "safety_concerns_girls"
  output <- lby_msna2021 %>%
    get_question_columns(question_label = question_label, group_sep = "\\/")

  expected_columns <- lby_msna2021_choices %>%
    dplyr::filter(.data[["list_name"]] == "pr_9_safety_concerns_girls_list") %>%
    dplyr::pull(name) %>%
    sapply(., FUN = function(x) {
      paste0(question_label, "/", x)
    }) %>%
    as.vector()

  expect_true(
    identical(
      output, expected_columns
    )
  )
})

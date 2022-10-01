test_that("changelog checks uniqueness of id columns", {
  tab_a <- tibble::tribble(
    ~id, ~city, ~a, ~b,
    1, "a", 1, 2,
    2, "b", 3, 4,
    2, "b", 3, 5,
  )

  testthat::expect_error(
    generate_changelog(tab_a, tab_a, id_cols = c(id, city)),
    "The provided id_cols do not uniquely identify each row"
  )
})

test_that("changelog checks that id columns are common across inputs", {
  tab_a <- tibble::tribble(
    ~id, ~city, ~a, ~b,
    1, "a", 1, 2,
    2, "b", 3, 4,
    3, "c", 5, 6,
  )

  tab_b <- tibble::tribble(
    ~id, ~city, ~a, ~b,
    4, "a", 1, 2,
    5, "b", 3, 4,
    6, "c", 5, 6,
  )

  testthat::expect_error(
    generate_changelog(tab_a, tab_b, id_cols = c(id, city)),
    "The id_cols are not common across the two data frames"
  )
})

test_that("changelog checks that inputs have the same dimensions", {
  tab_a <- tibble::tribble(
    ~id, ~a, ~b,
    1, 1, 2,
    2, 3, 4,
    3, 5, 6,
  )

  tab_b <- tibble::tribble(
    ~id, ~a, ~b, ~c,
    1, 1, 2, 1,
    2, 3, 4, 1,
    3, 5, 4, 1
  )
  testthat::expect_error(
    generate_changelog(.x = tab_a, .y = tab_b, id_cols = id),
    "[ncol|nrow].*not equal to [ncol|nrow].*"
  )

  # NOTE: Although this tests accounts for the possibility of
  # the number of rows being different, such a case should
  # be rejected by earlier tests where either the IDs do not uniquely
  # identify rows, or some id values are not shared across the two data
  # frames
})

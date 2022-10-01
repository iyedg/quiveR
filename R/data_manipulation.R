#' Generate a changelog from two data frames.
#'
#' @param .x data frame x
#' @param .y data frame y
#' @param id_cols columns common between x and y that uniquely identify observations
#' @param names_to name of the column containing the pivoted column names
#' @param .x_values_to name of the column containing values from x
#' @param .y_values_to name of the column containing values from y
#'
#' @return data frame
#' @export
generate_changelog <- function(.x, .y, id_cols,
                               names_to = "question",
                               .x_values_to = "raw_value",
                               .y_values_to = "transformed_value") {
  assertthat::assert_that(
    nrow(dplyr::distinct(dplyr::select(.x, {{ id_cols }}))) == nrow(.x) &
      nrow(dplyr::distinct(dplyr::select(.y, {{ id_cols }}))) == nrow(.y),
    msg = "The provided id_cols do not uniquely identify each row"
  )

  assertthat::assert_that(
    isTRUE(dplyr::all_equal(
      dplyr::distinct(dplyr::select(.x, {{ id_cols }})),
      dplyr::distinct(dplyr::select(.y, {{ id_cols }})),
    )),
    msg = "The id_cols are not common across the two data frames"
  )


  # TODO: ideally failure here would raise a warning.
  # For now this capability is not available through assertthat
  assertthat::assert_that(
    nrow(.x) == nrow(.y), ncol(.x) == ncol(.y),
  )

  long_x <- .x %>%
    tidyr::pivot_longer(-{{ id_cols }},
      names_to = names_to,
      values_to = .x_values_to,
      values_transform = as.character
    )

  long_y <- .y %>%
    tidyr::pivot_longer(-{{ id_cols }},
      names_to = names_to,
      values_to = .y_values_to,
      values_transform = as.character
    )

  changelog_df <- dplyr::right_join(long_x, long_y) %>%
    dplyr::filter(.data[[.x_values_to]] != .data[[.y_values_to]])
  return(changelog_df)
}

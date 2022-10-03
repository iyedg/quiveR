#' Download survey responses as a dataset
#' @param server_url URL of the KoBo Server
#' @param type Filetype for the export
#' @param lang Language of the labels used in the export. Can be 'xls'
#' @param fields_from_all_versions Whether to include fields from all versions of the survey
#' @param group_sep The character separator for groups
#' @param asset_uid The UUID of the survey
#' @param user_name The User Name
#' @param user_password  The Password
#' @param file_path The file path to which the dataset is saved
#' @param max_retries The number of retries when downloading
#'
#' @return NULL
#' @export
#'
#' @examples
kobo_download_dataset <- function(server_url = "https://kobo.humanitarianresponse.info",
                                  type = "xls",
                                  lang = "English",
                                  fields_from_all_versions = TRUE,
                                  group_sep = "/",
                                  asset_uid,
                                  user_name,
                                  user_password,
                                  file_path,
                                  max_retries = 12) {
  api_url_export <- glue::glue("{server_url}/exports/")
  api_url_asset <- glue::glue("{server_url}/assets/{asset_uid}/")

  response <- httr2::request(api_url_export) %>%
    httr2::req_body_form(
      source = api_url_asset,
      type = type,
      lang = lang,
      fields_from_all_versions = fields_from_all_versions,
      group_sep = group_sep
    ) %>%
    httr2::req_auth_basic(username = user_name, password = user_password) %>%
    httr2::req_perform()

  assertthat::assert_that(
    response$status_code == 201,
    msg = "Failed to initiate export"
  )

  response_content <- response %>%
    httr2::resp_body_json()

  cli::cli_alert_success("An export has been initiated successfully")

  retries <- 0
  repeat {
    retries <- retries + 1
    Sys.sleep(5)
    cli::cli_alert_info(glue::glue("[{retries} / {max_retries}] Checking if the export is ready ..."))

    export_response <- httr2::request(response_content$url) %>%
      httr2::req_auth_basic(user_name, user_password) %>%
      httr2::req_perform()

    export_response_content <- export_response %>%
      httr2::resp_body_json()

    if (export_response_content$status == "complete") {
      break
    }
    if (retries >= max_retries) {
      stop("Maximum number of retries exceeded")
    }
  }

  cli::cli_alert_info("Downloading the dataset")
  resp <- httr2::request(export_response_content$result) %>%
    httr2::req_auth_basic(user_name, user_password) %>%
    httr2::req_perform(path = file_path)
  assertthat::assert_that(resp$status == 200)
  cli::cli_alert_success(glue::glue("Dataset downloaded successfuly to {file_path}"))
}

## code to prepare `lby_msna` dataset goes here
library(magrittr)

msna_url <- "https://www.impact-repository.org/document/reach/d12e44b6/REACH_LBY_Dataset_2105b_August-2021.xlsx"

file_path <- tempfile(fileext = "xlsx")

httr::GET(msna_url, httr::write_disk(file_path, overwrite = TRUE))

lby_msna2021 <- readxl::read_excel(file_path,
  sheet = "Clean Data",
  skip = 1,
  na = c("NA", "^$", "^\\s+$")
) %>%
  janitor::remove_empty(which = "cols") %>%
  dplyr::select(-all_of(c("consent_1", "_notes", "_status", "_submitted_by")))

usethis::use_data(lby_msna2021,
  overwrite = TRUE,
  version = 3
)

lby_msna2021_survey <- readxl::read_excel(file_path,
  sheet = "Kobo survey",
  na = c("NA")
) %>%
  janitor::remove_empty(which = "cols") %>%
  janitor::clean_names()

usethis::use_data(lby_msna2021_survey, overwrite = TRUE, version = 3)

lby_msna2021_choices <- readxl::read_excel(file_path,
  sheet = "Kobo choices",
  na = c("NA")
) %>%
  janitor::remove_empty(which = "cols") %>%
  janitor::clean_names()

usethis::use_data(lby_msna2021_choices, overwrite = TRUE, compress = "bz2", version = 3)

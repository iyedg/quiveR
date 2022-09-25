## code to prepare `lby_msna` dataset goes here
msna_url <- "https://www.impact-repository.org/document/reach/d12e44b6/REACH_LBY_Dataset_2105b_August-2021.xlsx"

file_path <- tempfile(fileext = "xlsx")

httr::GET(msna_url, httr::write_disk(file_path, overwrite = TRUE))

lby_msna <- readxl::read_excel(file_path,
  sheet = "Clean Data",
  skip = 1,
  na = c("NA")
) %>%
  select(-contains("/")) %>% # The columns expanded from select multiple questions are not needed
  janitor::remove_empty(which = "cols")

usethis::use_data(lby_msna, overwrite = TRUE, compress = "bzip2", version = 3)

lby_msna_survey <- readxl::read_excel(file_path,
  sheet = "Kobo survey",
  na = c("NA")
) %>%
  janitor::remove_empty(which = "cols") %>%
  janitor::clean_names()

usethis::use_data(lby_msna_survey, overwrite = TRUE, compress = "bzip2", version = 3)

lby_msna_choices <- readxl::read_excel(file_path,
  sheet = "Kobo choices",
  na = c("NA")
) %>%
  janitor::remove_empty(which = "cols") %>%
  janitor::clean_names()

usethis::use_data(lby_msna_choices, overwrite = TRUE, compress = "bzip2", version = 3)

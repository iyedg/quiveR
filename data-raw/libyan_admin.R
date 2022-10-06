library(dplyr)
library(magrittr)
library(here)
library(openxlsx)

admin_codes_path <- here("data-raw/lby_pcodes_kobo.xlsx")
admin_codes_wb <- loadWorkbook(admin_codes_path)

######### Regions
lby_regions <- readWorkbook(admin_codes_wb, sheet = "admin1") %>%
  janitor::clean_names() %>%
  rename(
    region_id = adm1_geo_2,
    region_name_en = adm1_geodi,
    region_name_ar = adm1_geo_1
  ) %>%
  mutate(
    region_id = stringr::str_replace(region_id, "LY", "LBY"),
    across(.fns = stringr::str_squish)
  )

usethis::use_data(lby_regions, overwrite = TRUE)


######### Districts
lby_districts <- readWorkbook(admin_codes_wb, sheet = "admin2") %>%
  janitor::clean_names() %>%
  rename(
    region_id = adm1_pcode,
    district_id = adm2_pcode,
    district_name_en = adm2_name_en,
    district_name_ar = adm2_name_ar
  ) %>%
  mutate(
    region_id = stringr::str_replace(region_id, "LY", "LBY"),
    district_id = stringr::str_replace(district_id, "LY", "LBY"),
    across(.fns = stringr::str_squish)
  ) %>%
  select(-contains("adm1")) %>%
  relocate(region_id, .before = district_id)

usethis::use_data(lby_districts, overwrite = TRUE)

######### Municipalities
lby_municipalities <- readWorkbook(admin_codes_wb, sheet = "admin3") %>%
  janitor::clean_names() %>%
  rename(
    region_id = adm1_geo_2,
    district_id = adm2_pcode,
    municipality_id = adm3_pcode,
    municipality_name_en = adm3_name_en,
    municipality_name_ar = adm3_name_ar
  ) %>%
  mutate(
    region_id = stringr::str_replace(region_id, "LY", "LBY"),
    district_id = stringr::str_replace(district_id, "LY", "LBY"),
    municipality_id = stringr::str_replace(municipality_id, "LY", "LBY"),
    across(.fns = stringr::str_squish)
  ) %>%
  select(-contains("adm1"), -contains("adm2")) %>%
  relocate(district_id, .before = municipality_id) %>%
  relocate(region_id, .before = district_id)

usethis::use_data(lby_municipalities, overwrite = TRUE)

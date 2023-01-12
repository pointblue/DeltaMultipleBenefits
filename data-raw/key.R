## code to prepare `key` dataset goes here

key = readxl::read_excel('C:/Users/kdybala/Documents/delta_scenarios/GIS/VEG_key.xlsx') %>%
  dplyr::select(CODE_BASELINE, CODE_NAME, CLASS, SUBCLASS, DETAIL, LABEL, COLOR)

usethis::use_data(key, overwrite = TRUE)

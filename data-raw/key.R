## code to prepare `key` dataset goes here

key = readxl::read_excel('C:/Users/kdybala/Documents/R_projects/delta_tima/dat_clean/veg_key.xlsx') |>
  dplyr::select(CODE_BASELINE, CODE_NAME, CLASS, SUBCLASS, DETAIL, LABEL, COLOR) |>
  dplyr::arrange(CODE_BASELINE)

usethis::use_data(key, overwrite = TRUE)

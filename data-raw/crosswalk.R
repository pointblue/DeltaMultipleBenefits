## code to prepare datasets describing land cover crosswalks to each set of
## species distribution models

key = readxl::read_excel('C:/Users/kdybala/Documents/R_projects/delta_tima/dat_clean/veg_key.xlsx', 'key') |>
  dplyr::select(CODE_NUM, NAME_FULL, CLASS, SUBCLASS, DETAIL, LABEL, COLOR) |>
  dplyr::arrange(CODE_NUM)

usethis::use_data(key, overwrite = TRUE)

predictors_riparian = readxl::read_excel('C:/Users/kdybala/Documents/R_projects/delta_tima/dat_clean/veg_key.xlsx',
                                         'predictors_riparian',
                                         na = 'NA') |>
  dplyr::arrange(CODE_NUM)
usethis::use_data(predictors_riparian, overwrite = TRUE)

predictors_waterbirds_fall = readxl::read_excel(
  'C:/Users/kdybala/Documents/R_projects/delta_tima/dat_clean/veg_key.xlsx',
  'predictors_waterbirds_fall', na = 'NA') |>
  dplyr::arrange(CODE_NUM)
usethis::use_data(predictors_waterbirds_fall, overwrite = TRUE)

predictors_waterbirds_win = readxl::read_excel(
  'C:/Users/kdybala/Documents/R_projects/delta_tima/dat_clean/veg_key.xlsx',
  'predictors_waterbirds_win', na = 'NA') |>
  dplyr::arrange(CODE_NUM)
usethis::use_data(predictors_waterbirds_win, overwrite = TRUE)

predictors_tima = readxl::read_excel(
  'C:/Users/kdybala/Documents/R_projects/delta_tima/dat_clean/veg_key.xlsx',
  'predictors_tima', na = 'NA') |>
  dplyr::arrange(CODE_NUM)
usethis::use_data(predictors_tima, overwrite = TRUE)


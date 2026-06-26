## code to prepare `metrics` dataset goes here

#metrics = readr::read_csv('C:/Users/kdybala/Documents/R_projects/delta_scenarios/output/metrics.csv')
metrics = readr::read_csv('C:/Users/kdybala/Documents/R_projects/delta_tima/dat_clean/metrics_v2.csv')

usethis::use_data(metrics, overwrite = TRUE)

## code to prepare `metrics` dataset goes here

metrics = readr::read_csv('C:/Users/kdybala/Documents/delta_scenarios/output/metrics.csv')

usethis::use_data(metrics, overwrite = TRUE)

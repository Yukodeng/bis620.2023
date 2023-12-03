## code to prepare `DATASET` dataset goes here

accel = readRDS("accel.rds")
# studies = readRDS("studies.rds")
# conditions = readRDS("conditions.rds")
# interventions = readRDS("interventions.rds")
# sponsors = readRDS("sponsors.rds")


usethis::use_data(accel, overwrite = TRUE)
# usethis::use_data(studies, overwrite = TRUE)
# usethis::use_data(sponsors, overwrite = TRUE)
# usethis::use_data(interventions, overwrite = TRUE)
# usethis::use_data(conditions, overwrite = TRUE)

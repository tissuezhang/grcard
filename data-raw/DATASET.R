## code to prepare `DATASET` dataset goes here

usethis::use_data(birthday_data, compress = "xz", overwrite = TRUE)
readr::write_csv(birthday_data, path = "inst/extdata/birthday_data.csv")

usethis::use_data(christmas_data, compress = "xz")
readr::write_csv(christmas_data, path = "inst/extdata/christmas_data.csv")



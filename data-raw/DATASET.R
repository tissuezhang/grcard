## code to prepare `DATASET` dataset goes here

usethis::use_data(birthday_data, compress = "xz", overwrite = TRUE)
readr::write_csv(birthday_data, path = "inst/extdata/birthday_data.csv")

#' Read CSV duplicate
#'
#' @param file path to file name
#'
#' @return a table
#' @export
#' @importFrom readr read_csv
#' @examples
#' birthday_data = system.file("extdata", "birthday_data.csv", package="grcard")
#' data_read(birthday_data)
data_read = function(file){
  readr::read_csv(file)
}

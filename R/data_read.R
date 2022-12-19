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

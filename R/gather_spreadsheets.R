#' @title Read data from excel spreadsheet
#' @param input_path Path to input Excel spreadsheet
#' @return a data.frame
read_data <- function(input_path) {
  my_data <- readxl::read_excel(input_path)
  my_data$new_column <- NA
  return(my_data)
}

#' @title pre process
preprocess_data <- function(input_data) {
  input_data$a <- 10
  return(input_data)
}

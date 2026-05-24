#' Split a string
#'
#' @param string A character vector with, at most, one element containing the link to the csv.
#' @inheritParams readr::read_csv
#'
#' @return The loaded dataset in the form of a dataframe.
#'
#' @examples
#' load_data()
#'
#' y <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_year.csv'
#' str_split_one(y)
#' 
#' @export

load_data <- function(link = 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_year.csv'){
  read_csv(link)
}
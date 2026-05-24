#' Load in TidyTuesday Data
#'
#' @param link A character vector with one element containing the link to the csv.

#' @inheritParams readr::read_csv
#'
#' @return The loaded dataset in the form of a dataframe.
#' 
#' @importFrom readr read_csv
#' 
#' @examples
#' load_data()
#'
#' y <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_month.csv'
#' load_data(y)
#' 
#' @export

load_data <- function(link = 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_year.csv'){
  read_csv(link)
}
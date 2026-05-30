#' Load in Metadata
#'
#' @return The loaded metadata in the form of a dataframe.
#'
#' @importFrom here here
#'
#' @examples
#' load_data()
#'
#' @export

load_data <- function(){load(here::here("inst", "metadata.RData"))}

#' Load in Metadata
#'
#' @return The loaded metadata in the form of a dataframe.
#'
#' @importFrom arrow read_parquet
#'
#' @export
#'
#' @examples
#' load_data()

load_data <- function() {
  #load(here("inst", "metadata.RData"))
  #load(system.file("metadata.RData", package = "mmR"))
  arrow::read_parquet(system.file("metadata.parquet", package = "mmR"))
  #metadata
}

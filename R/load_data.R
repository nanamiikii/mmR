#' Load in Metadata
#'
#' @return The loaded metadata in the form of a dataframe.
#'
#' @importFrom here here
#'
#' @export
#'
#' @examples
#' load_data()

load_data <- function() {
  #load(here("inst", "metadata.RData"))
  load(system.file("metadata.RData", package = "mmR"))
  metadata
}

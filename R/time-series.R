#' Generates a time series plot of a given parameter over a specified time range, with optional filtering.
#'
#' @param parameter A character string specifying the parameter to be plotted (Default:, "Measles")
#' @param start_year A character string specifying the starting year for the time series (Default: "2012")
#' @param end_year A character string specifying the ending year for the time series (Default: "2024")
#' @param title_ A character string specifying the title of the plot (Default: NULL)
#' @param filepath A character string specifying the file path where the generated *.gif file will be saved (Default: "time_lapse.gif")
#' @param filter_vector An optional vector of character strings to filter the data by specific categories or groups (Default: NULL)
#' @param palette A vector denoting the colors of the plotted parameter
#' @return A *.gif file showing the animated time series of the specified parameter over the given time range.
#'
#' @examples
#' # Animate MCV2 vaccination coverage over time (all countries, default years)
#' time_lapse_plot(parameter = "mcv2")
#'
#' # Narrow to a single country and custom year range
#' time_lapse_plot(
#'   parameter     = "mcv2",
#'   start_year    = "2015",
#'   end_year      = "2022",
#'   filter_vector = c(country = "Mongolia"),
#'   title_        = "MCV2 Coverage in Mongolia (2015-2022)"
#' )
#'
#' # Animate Measles incidence rate for the Western Pacific region
#' time_lapse_plot(
#'   parameter     = "measles_incidence_rate_per_1000000_total_population",
#'   filter_vector = c(region_s = "WPRO")
#' )
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line labs scale_y_continuous scale_color_manual scale_x_continuous theme_minimal theme element_text
#' @importFrom gganimate transition_reveal
#' @importFrom scales label_percent
#' @importFrom gifski gifski
#' @importFrom glue glue
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @export
########## plotting function ##########

time_lapse_plot <- function(parameter = "measles_total",
                            start_year = "2012",
                            end_year = "2024",
                            title_ = glue("{parameter} from {start_year} to {end_year}"),
                            filepath = NULL,
                            filter_vector = NULL,
                            palette = c("#001E4D", "#F4A324")) {

  # Load metadata
  data <- load_data()

  # Parameter checks
  if (!parameter %in% colnames(data)) {
    stop(glue(
      "`parameter` must be a column name in the dataset. '{parameter}' not found. ",
      "Use colnames(load_data()) to see available columns."
    ))
  }

  # condition check for filtering, establishing dataframe that will be graphed.
  if (!is.null(filter_vector)) {
    graph_data <- filter_data(data, filter_vector)

  } else {
    graph_data <- data
  }

  # filtering for specified start and end time, then creating the plot.
  plot <- graph_data |>
    filter(year >= as.numeric(start_year),
           year <= as.numeric(end_year)
    ) |>
    ggplot(aes(x = year,
               y = parameter
    )
    ) +
    geom_point(size = 3) +
    geom_line(linewidth = 2) +
    transition_reveal(year) +
    labs(
      x = "Year",
      y = "",
      subtitle = "Year being shown: {round(frame_along, 0)}",
      title = title_
    ) +
    scale_color_manual(values = palette
    ) +
    scale_x_continuous(breaks = seq(as.numeric(start_year), as.numeric(end_year), by = 2)) +
    theme_minimal() +
    theme(axis.title.x = element_text(size = 15),
          axis.text = element_text(size = 15),
          legend.text = element_text(size = 15),
          plot.subtitle = element_text(size = 15),
          plot.title = element_text(size = 15)
    )
}


########## helper function ##########
filter_data <- function(data, filter_vector) {

  # Parameter checks
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }

  if (!is.character(filter_vector) || length(filter_vector) == 0) {
    stop("`filter_vector` must be a non-empty named character vector, e.g. c(country = \"Mongolia\").")
  }

  if (is.null(names(filter_vector)) || any(names(filter_vector) == "")) {
    stop("`filter_vector` must be fully named, e.g. c(country = \"Mongolia\") or c(region_s = \"AMRO\").")
  }

  # Check that all names are valid column names in the data
  invalid_cols <- names(filter_vector)[!names(filter_vector) %in% colnames(data)]
  if (length(invalid_cols) > 0) {
    stop(glue(
      "The following name(s) in `filter_vector` are not columns in the dataset: ",
      "{paste(invalid_cols, collapse = ', ')}. ",
      "Use colnames(load_data()) to see available columns."
    ))
  }

  # Check that each value exists in its respective column
  for (col in unique(names(filter_vector))) {
    vals        <- unname(filter_vector[names(filter_vector) == col])
    valid_vals  <- unique(as.character(data[[col]]))
    invalid_vals <- vals[!vals %in% valid_vals]
    if (length(invalid_vals) > 0) {
      stop(glue(
        "The following value(s) for `{col}` are not found in the data: ",
        "{paste(invalid_vals, collapse = ', ')}. ",
        "Use load_data() to view valid entries."
      ))
    }
  }

  # Filter sequentially: AND across different columns, OR within the same column
  for (col in unique(names(filter_vector))) {
    vals <- unname(filter_vector[names(filter_vector) == col])
    data <- data |> filter(.data[[col]] %in% vals)
  }

  data
}

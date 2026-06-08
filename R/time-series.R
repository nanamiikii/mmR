#' Time series plot of one or more parameters over a specified time range, with optional filtering.
#'
#' Data are aggregated by year (median by default; set \code{use_mean = TRUE} for mean)
#' after any \code{filter_vector} filtering is applied, so the plot always shows a single
#' summary line per parameter.
#'
#' @param parameters A character vector of column names to plot (Default: \code{"measles_total"}).
#'   All series are overlaid on the same graph.
#' @param start_year A numerical or numeric string specifying the starting year for the time series (Default: 2012)
#' @param end_year A numerical or numeric string specifying the ending year for the time series (Default: 2024)
#' @param title_ A character string specifying the title of the plot (Default: \code{glue("{paste(parameters, collapse = ', ')} from {start_year} to {end_year}")}
#' @param filter_vector An optional named character vector to filter the data by specific categories
#'   or groups (Default: \code{NULL}).
#' @param use_mean A logical; if \code{TRUE} the aggregation uses \code{mean}, otherwise
#'   \code{median} (Default: FALSE).
#' @param palette A character vector of colours, one per parameter in \code{parameters}.
#'   Recycled if shorter than the number of parameters (Default: \code{c("#001E4D", "#F4A324")}).
#' @return A ggplot / gganimate object showing the animated time series.
#'
#' @examples
#' # Single parameter – median by year (default)
#' time_lapse_plot(parameters = "mcv2")
#'
#' # Two parameters on the same graph
#' time_lapse_plot(
#'   parameters = c("mcv2", "measles_incidence_rate_per_1000000_total_population")
#' )
#'
#' # Narrow to a single country by applying categorical filter
#' time_lapse_plot(
#'   parameters    = "mcv2",
#'   start_year    = 2015,
#'   end_year      = 2022,
#'   filter_vector = c(country = "Mongolia"),
#'   title_        = "MCV2 Coverage in Mongolia (2015-2022)"
#' )
#'
#' # Use mean instead of median for the yearly summary
#' time_lapse_plot(parameters = "mcv2", use_mean = TRUE)
#'
#' @import dplyr
#' @import tidyr 
#' @import ggplot2
#' @import gifski
#' @import glue
#' @importFrom gganimate transition_reveal
#' @importFrom rlang .data
#' @importFrom furrr future_walk
#'
#' @export

########## plotting function ##########

time_lapse_plot <- function(parameters = "measles_total",
                            start_year = 2012,
                            end_year = 2024,
                            title_ = glue("{paste(parameters, collapse = ', ')} from {start_year} to {end_year}"),
                            filter_vector = NULL,
                            use_mean = FALSE,
                            palette = c("#001E4D", "#F4A324")) {

  # Load metadata
  data <- load_data()

  ##### Parameter checks #####
  message("checking parameters... [1/4]")
  invalid_params <- parameters[!parameters %in% colnames(data)]
  if (length(invalid_params) > 0) {
    stop(glue(
      "The following parameter(s) are not column names in the dataset: ",
      "{paste(invalid_params, collapse = ', ')}. ",
      "Use colnames(load_data()) to see available columns."
    ))
  }

  if (!start_year %in% data$year || !end_year %in% data$year) {
    stop(glue(
      "Inputted years are out of range. Year values must be between {min(data$year)} and {max(data$year)}."
    ))
  }

  ##### PREPARING GRAPHING DATA #####
  message("preparing graphing data... [2/4]")
  # Filter to year range first
  graph_data <- data |>
    filter(year >= as.numeric(start_year), year <= as.numeric(end_year))

  # Apply optional row filter
  if (!is.null(filter_vector)) {
    graph_data <- filter_data(graph_data, filter_vector)
  }

  # Pivot all requested parameters into long form for a single colour aesthetic
  graph_data <- graph_data |>
    select(year, all_of(parameters)) |>
    pivot_longer(cols = all_of(parameters), names_to = "parameter", values_to = "value")


  # logical argument to group by mean v. median (default is median)
  if (use_mean) {
    graph_data <- graph_data |>
      group_by(year, parameter) |>
      summarize(value = mean(value, na.rm = TRUE), .groups = "drop")
  } else {
    graph_data <- graph_data |>
      group_by(year, parameter) |>
      summarize(value = median(value, na.rm = TRUE), .groups = "drop")
  }

  # Recycle palette to cover all parameters
  n_params   <- length(parameters)
  pal_values <- setNames(rep_len(palette, n_params), parameters)

  #### PLOT PLOT PLOT #####
  message("preparing plot... [3/4]")
  plot <- graph_data |>
    ggplot(aes(x = year, y = value, color = parameter)) +
    geom_point(size = 3) +
    geom_line(linewidth = 2) +
    transition_reveal(year) +
    labs(
      x     = "Year",
      y     = "",
      color = "Parameter",
      subtitle = "Year being shown: {round(frame_along, 0)}",
      title = title_
    ) +
    scale_color_manual(values = pal_values) +
    scale_x_continuous(breaks = seq(as.numeric(start_year), as.numeric(end_year), by = 2)) +
    scale_y_continuous(limits = c(0, NA)) + 
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 15),
      axis.text    = element_text(size = 15),
      legend.text  = element_text(size = 15),
      plot.subtitle = element_text(size = 15),
      plot.title   = element_text(size = 15)
    )
  
  message("done! now generating *.gif... [4/4]")
  return(plot)
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

  # Check that each value exists in its respective column -- removed for loop
  future_walk(unique(names(filter_vector)), function(col) {
    vals         <- unname(filter_vector[names(filter_vector) == col])
    valid_vals   <- unique(as.character(data[[col]]))
    invalid_vals <- vals[!vals %in% valid_vals]
  
    if (length(invalid_vals) > 0) {
      stop(glue(
        "The following value(s) for `{col}` are not found in the data: ",
        "{paste(invalid_vals, collapse = ', ')}. ",
        "Use load_data() to view valid entries."
      ))
    }
  })

  # Filter sequentially: AND across different columns, OR within the same column
  # honestly, can't quite figure out how to use map here...
  for (col in unique(names(filter_vector))) {
    vals <- unname(filter_vector[names(filter_vector) == col])
    data <- data |> filter(.data[[col]] %in% vals)
  }

  data
}

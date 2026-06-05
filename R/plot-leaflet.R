#' Leaflet Plot
#'
#' @param var The variable to be plotted (mcv2, GDP_per_capita,
#' measles_incidence_rate_per_1000000_total_population)
#' @param low The color for the low end of the scale, as a string
#' @param high The color for the high end of the scale, as a string
#' @param format Whether or not to format the variable (i.e. percents, dollar
#' amounts, commas). Default is TRUE
#'
#' @import dplyr
#' @import leaflet
#' @importFrom rlang as_name ensym
#' @importFrom htmlwidgets onRender
#' @importFrom rnaturalearth ne_countries
#' @importFrom scales percent dollar comma rescale
#'
#' @returns An interactive leaflet plot
#' @export
#'
#' @examples
#' plot_leaflet(var = mcv2, low = "#ecf0f1", high = "#e74c3c")
#' plot_leaflet(var = GDP_per_capita, low = "#ecf0f1", high = "#1ccdaa")
#' plot_leaflet(var = measles_incidence_rate_per_1000000_total_population,
#' low = "#ecf0f1", high = "#f4a324", format = FALSE)
plot_leaflet <- function(var, low, high, format = TRUE) {
  var_name <- as_name(ensym(var))
  validate_var(var_name)

  validate_color(low)
  validate_color(high)

  if (!is.logical(format)) {
    stop("format should be logical")
  }

  metadata_2024 <- formatted_data(var_name, format)

  map_data <- ne_countries(scale = "medium", returnclass = "sf") |>
    left_join(metadata_2024, join_by(iso_a3_eh == iso3))

  pal <- colorNumeric(palette = c(low, high),
                      domain = c(0, 1),
                        na.color = "darkgray")

  var_label = switch(var_name,
                     mcv2 = "MCV2",
                     GDP_per_capita = "GDP per capita",
                     measles_incidence_rate_per_1000000_total_population = "Measles")

  map_data |>
    leaflet() |>
    setView(lng = 0, lat = 30, zoom = 1) |>
    onRender("function(el, x) {el.style.backgroundColor = '#3498db';}") |>
    addPolygons(fillColor = ~pal(scale_col),
                fillOpacity = 1,
                color = "white",
                weight = 1,
                opacity = 1,
                label = ~paste0(name, " ", var_label, ": ",
                                ifelse(is.na(scale_col), "No Data", label_col)))
}

#' Variable Name Checker
#'
#' @param var_name The name of the variable to checked
#'
#' @examples
#' mmR:::validate_var("mcv2")
validate_var <- function(var_name) {
  var_options <- c("mcv2",
                   "GDP_per_capita",
                   "measles_incidence_rate_per_1000000_total_population")

  if (!var_name %in% var_options) {
    stop("Please choose one of:
             mcv2,
             GDP_per_capita,
             measles_incidence_rate_per_1000000_total_population")
  }
}

#' Color Checker
#'
#' @param color The color input to be checked
#'
#' @importFrom grDevices col2rgb
#'
#' @examples
#' mmR:::validate_color("#ecf0f1")
validate_color <- function(color) {
  if (!is.character(color)){
    stop("both low and high should be strings")
  }

  tryCatch({
      col2rgb(color)
      TRUE
    },
    error = function(e) {
      stop("check that both low and high are actual colors")
    })
}

#' Creating Formatted Data
#'
#' @param var_name The name of the variable chosen
#' @param format Whether or not to format the variable (i.e. percents, dollar
#' amounts, commas)
#'
#' @import dplyr
#'
#' @examples
#' mmR:::formatted_data("mcv2", TRUE)
formatted_data <- function(var_name, format) {
  load_data() |>
      filter(year == 2024) |>
      mutate(label_col = if (format) {
        switch(var_name,
               "mcv2" = percent(.data[[var_name]]),
               "GDP_per_capita" = dollar(.data[[var_name]]),
               "measles_incidence_rate_per_1000000_total_population" = comma(.data[[var_name]]))
        }
                         else .data[[var_name]],
             scale_col = if (var_name != "mcv2") {
               rescale(.data[[var_name]], to = c(0, 1))
               }
                         else .data[[var_name]])
}

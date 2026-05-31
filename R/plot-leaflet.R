#' Leaflet Plot
#'
#' @param var The variable to be plotted (mcv2, GDP_per_capita,
#' measles_incidence_rate_per_1000000_total_population)
#' @param low The color for the low end of the scale, as a string
#' @param high The color for the high end of the scale, as a string
#' @param format How the parameter should be formatted ("percent", "dollar",
#' "comma")
#' @param normalize Whether or not the variable should be normalized, should be
#' TRUE for variables not on a 0-1 scale. Default is FALSE
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
#' plot_leaflet(var = mcv2, low = "#ecf0f1", high = "#e74c3c",
#' format = "percent")
#' plot_leaflet(var = GDP_per_capita, low = "#ecf0f1", high = "#1ccdaa",
#' format = "dollar", normalize = TRUE)
#' plot_leaflet(var = measles_incidence_rate_per_1000000_total_population,
#' low = "#ecf0f1", high = "#f4a324", format = "comma", normalize = TRUE)
plot_leaflet <- function(var, low, high, format, normalize = FALSE) {
  var_name <- as_name(ensym(var))
  validate_var(var_name)

  stopifnot(is.character(low),
            is.character(high),
            is.character(format),
            is.logical(normalize))

  if (!format %in% c("percent", "dollar", "comma")) {
    stop('Please choose one of: "percent", "dollar", "comma"')
  }

  metadata_2024 <- load_data() |>
    filter(year == 2024) |>
    mutate(label_col = switch(format,
                              "percent" = percent({{var}}),
                              "dollar"  = dollar({{var}}),
                              "comma"   = comma({{var}})),
           scale_col = if (normalize) {rescale({{var}}, to = c(0, 1))}
                       else {{var}})

  map_data <- ne_countries(scale = "medium", returnclass = "sf") |>
    left_join(metadata_2024, join_by(iso_a3_eh == iso3))

  pal <- colorNumeric(palette = c(low, high),
                      domain = c(0, 1),
                        na.color = "darkgray")

  # parameter = switch(var_name,
  #                    "mcv2" = "MCV2",
  #                    "GDP_per_capita" = "GDP per capita",
  #                    "measles_incidence_rate_per_1000000_total_population" = "Measles")

  map_data |>
    leaflet() |>
    setView(lng = 0, lat = 30, zoom = 1) |>
    onRender("function(el, x) {el.style.backgroundColor = '#3498db';}") |>
    addPolygons(fillColor = ~pal(scale_col),
                fillOpacity = 1,
                color = "white",
                weight = 1,
                opacity = 1,
                label = ~paste0(name, ": ",
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

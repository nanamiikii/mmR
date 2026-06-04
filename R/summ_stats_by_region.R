#' Summary statistics of a variable
#'
#' Helper function that calculates summary statistics based on the type of variable
#'
#' @param x Variable of the form of a vector
#'
#' @returns A tibble of columns for the variable, where each column is a different measure
#'
#' @examples
#' # summary statistics for the region variable
#' summ_stats(load_data()$region)

generate_summary_stats <- function(x) {

  # if x contains numeric values
  # return a tibble with the mean, med, sd, IQR, and n observed
  if (is.numeric(x)) {
    out_df <- dplyr::tibble(
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      stdev = sd(x, na.rm = TRUE),
      iqr = IQR(x, na.rm = TRUE),
      n = sum(!is.na(x)) # length won't work bc of NAs
    )
    # if x contains categorical, return tibble with # of levels in variable
  } else if (is.factor(x) || is.character(x)) {
    out_df <- dplyr::tibble(
      n_levels = n_distinct(x)
    )
  } else {
    stop("Inputted values are not numeric or categorical.")
  }

  #return the data frame.
  out_df
}

#' Summary statistics for a specified variable aggregated by region or country
#'
#' Helper function used to calculate summary statistics for a specified variable
#'
#' @param df Data frame
#' @param col Some column of interest to summarize by
#' @param mode Mode to aggregate by
#'
#' @returns A tibble of summary statistics
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#' # Generate sample statistics for number of measles by region
#' summarize_stats(load_data(), measles_total, region)

summarize_stats_by <- function(df, col = measles_lab_confirmed, mode = region) {

  #if (!(mode %in% c("region", "country", "iso3", "year"))){
   # stop("Please select one of the available modes (region, country, iso3, year)")
  #}

  df |>
    dplyr::select({{mode}}, {{col}}) |>
    dplyr::group_by({{mode}}) |>
    #group_map removed the group column, so to retain that, group_modify.
    # See second example: https://dplyr.tidyverse.org/reference/group_map.html
    dplyr::group_modify( ~ generate_summary_stats(.x |>
                                        dplyr::pull({{col}}))) |>
    dplyr::bind_rows()
}


#' Summary statistics table
#'
#' Makes a gt table for top n countries for a region for the specified parameter.
#' This is similar to dashboard table on Mongolia panel.
#'
#' @param r Region of interest
#' @param summ_param Some parameter of interest
#' @param number Number of countries to include
#'
#' @returns A gt table of formatted summary statistics for a specified variable for a region
#'
#' @import dplyr
#' @import gt
#'
#' @export

summarize_stats_by_region <- function(r, summ_param, number = 5){

  validate_region(r)
  validate_var(summ_param)

  dat <- load_data() |>
    dplyr::filter(!(is.na(region)),
                  !(is.na(country))
                  ) |>
    dplyr::filter(region == r)

  summarize_stats_by(dat,
                  {{summ_param}},
                  mode = country
                  ) |>
    dplyr::ungroup() |>
    dplyr::slice_max(mean, n = number) |>
    dplyr::arrange(-mean) |>
    gt::gt() |>
    gt::fmt_number(columns = -c(country, n),
               decimals = 2) |>
    gt::cols_label("country" = "Country",
               "mean" = "Mean",
               "median" = "Median",
               "stdev" = "Standard Deviation",
               "iqr" = "IQR",
               "n" = "Sample Size",
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_style(
      style = gt::cell_text(style = "italic", decorate = "underline"),
      locations = gt::cells_row_groups()
    ) |>
    gt::tab_header(
      title = gt::md(
        "Summary Statistics for the Specified Variable (Region: {r})" |> stringr::str_glue()
        )
    ) |>
    gt::data_color(
      columns = "mean",
      method = "numeric",
      # #palette = c("#CCEEDF", "#BAE8D4", "#A8E2CA",
      # #           "#96DCBF", "#83D6B5", "#6ED0AB",
      # #          "#57CAA1", "#3BC397", "#00BD8D"
      # #         ),
      palette = c(
        "#FFE5C6", "#FFDDB3", "#FFD5A0",
        "#FFCC8D", "#FEC47A", "#FCBC67",
        "#FAB353", "#F7AB3E", "#F4A324"
      )
    )

}

#' Region Name checker
#'
#' Helper function to validate region names
#'
#' @param region_name Region to checked

validate_region <- function(region_name) {

  region_options <- c("Africa",
                      "Americas",
                      "Eastern Mediterranean Europe",
                      "South East Asia",
                      "Western Pacific"
                      )

  if (!region_name %in% region_options) {
    stop("Please choose one of the regions (Africa, Americas, Eastern Mediterranean Europe, South East Asia, Western Pacific)")
  }

}

#' Variable Name checker
#'
#' Helper function to validate variables
#'
#' @param var_name Variable to checked

validate_var <- function(var_name) {

  var_options <- load_data() |> names()

  if (!var_name %in% var_options) {
    stop("Please select one of the available variables of the metadata")
  }
}

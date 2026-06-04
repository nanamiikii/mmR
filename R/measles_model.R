#' Modeling function
#'
#' Makes a gls model using number of measles cases per million population as the dependent variable.
#' Since the model uses yearly data, the model utilizes an autocorrelation structure for the errors using year as the covariate.
#'
#' @param variables Vector of explanatory variables.
#'
#' @returns Model of class `gls`.
#'
#' @importFrom nlme gls corAR1
#'
#' @export
#'
#' @examples
#' # Make a model using `GDP_per_capita` and `mcv2` as explanatory variables
#' model_form(c("GDP_per_capita", "mcv2"))

model_form <- function(variables){

  if ("measles_incidence_rate_per_1000000_total_population" %in% variables){
    stop("Variable `measles_incidence_rate_per_1000000_total_population` is not able to be used for modeling.")
  }

  variables <- str_sort(variables)

  gls(

    reformulate(variables, "measles_incidence_rate_per_1000000_total_population"),

    data = load_data() |> mutate("GDP_per_capita" = log(GDP_per_capita)),

    na.action = na.omit,

    correlation = corAR1(form = ~ year | region / country)
  )

}

#' Make a table
#'
#' Generates a `gt` table that formats coefficients for a model created using the function `model_form`.
#' See documentation for `model_form` for detailed information about the model.
#'
#' @param variables Vector of explanatory variables.
#'
#' @returns Generated `gt` table
#'
#' @import stringr
#' @importFrom car Anova
#' @import gt
#'
#' @export
#'
#' @examples
#' # Generate a table for a model using `GDP_per_capita` and `mcv2` as explanatory variables
#' model_tab(c("GDP_per_capita", "mcv2"))
#'

model_tab <- function(variables){

  variables <- str_sort(variables)

  m <- model_form(variables)

  ma <- car::Anova(m)
  v <- ma$`Pr(>Chisq)`[stringr::str_which(variables, "region")] |> round(3)

  s <- summary(m)$tTable |>
    as_tibble(rownames = "var")

  if ("region" %in% variables){
    s <- s |>
      mutate(region = ifelse(
        stringr::str_detect(var, "region"),
        "Region (Africa as baseline) (overall p-value = {v})" |> stringr::str_glue(),
        "Quantitative"
        )
      ) |>
      group_by(region)
  }

  s |>
    mutate(var = var |>
             str_remove("region") |>
             str_replace("log_GDP_per_capita", "log(GDP per capita ($))") |>
             str_replace("mcv2", "MCV2")
    ) |>
    gt::gt() |>
    gt::fmt_number(columns = c("Value", "Std.Error", "t-value"),
               decimals = 3
    ) |>
    gt::fmt_number(columns = c("p-value"),
               decimals = 3
    ) |>
    #gt::cols_align(align = "left", columns = region) |>
    gt::cols_label("var" = "Variable",
               "Value" = "Estimated coefficient"
    ) |>
    gt::tab_style(
      style = cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_style(
      style = cell_text(style = "italic", decorate = "underline"),
      locations = gt::cells_row_groups()
    ) |>
    tab_header(
      title = md("Estimated coefficients for modeled variables")
    )

}

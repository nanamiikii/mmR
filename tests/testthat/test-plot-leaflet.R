test_that("var validation works", {
  expect_silent(validate_var("mcv2"))
  expect_silent(validate_var("GDP_per_capita"))
  expect_silent(validate_var("measles_incidence_rate_per_1000000_total_population"))

  expect_error(validate_var("sleep_rate"))
})

test_that("leaflet plot works", {
  leaflet_plot_mcv2 <- plot_leaflet(var = mcv2,
                                    low = "#ecf0f1",
                                    high = "#e74c3c",
                                    format = "percent")
  leaflet_plot_gdp <- plot_leaflet(var = GDP_per_capita,
                                   low = "#ecf0f1",
                                   high = "#1ccdaa",
                                   format = "dollar",
                                   normalize = TRUE)
  leaflet_plot_measles <- plot_leaflet(var = measles_incidence_rate_per_1000000_total_population,
                                       low = "#ecf0f1",
                                       high = "#f4a324",
                                       format = "comma",
                                       normalize = TRUE)

  expect_s3_class(leaflet_plot_mcv2, "leaflet")
  expect_s3_class(leaflet_plot_gdp, "leaflet")
  expect_s3_class(leaflet_plot_measles, "leaflet")
})

test_that("leaflet plot checks inputs", {
  expect_error(plot_leaflet(var = mcv2,
                            low = 1,
                            high = 2,
                            format = "percent"))
  expect_error(plot_leaflet(var = GDP_per_capita,
                            low = "#ecf0f1",
                            high = "#1ccdaa",
                            format = "something else",
                            normalize = TRUE))
  expect_error(plot_leaflet(var = measles_incidence_rate_per_1000000_total_population,
                            low = "#ecf0f1",
                            high = "#f4a324",
                            format = "comma",
                            normalize = "TRUE"))
})

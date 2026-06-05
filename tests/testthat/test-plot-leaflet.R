test_that("leaflet plot works", {
  leaflet_plot_mcv2 <- plot_leaflet(var = mcv2,
                                    low = "#ecf0f1",
                                    high = "#e74c3c")
  leaflet_plot_gdp <- plot_leaflet(var = GDP_per_capita,
                                   low = "#ecf0f1",
                                   high = "#1ccdaa",
                                   format = TRUE)
  leaflet_plot_measles1 <- plot_leaflet(var = measles_incidence_rate_per_1000000_total_population,
                                        low = "#ecf0f1",
                                        high = "#f4a324",
                                        format = TRUE)
  leaflet_plot_measles2 <- plot_leaflet(var = measles_incidence_rate_per_1000000_total_population,
                                        low = "#ecf0f1",
                                        high = "#f4a324",
                                        format = FALSE)

  expect_s3_class(leaflet_plot_mcv2, "leaflet")
  expect_s3_class(leaflet_plot_gdp, "leaflet")
  expect_s3_class(leaflet_plot_measles1, "leaflet")
  expect_s3_class(leaflet_plot_measles2, "leaflet")
})

test_that("leaflet plot checks inputs", {
  expect_error(plot_leaflet(var = something,
                            low = 1,
                            high = 2))
  expect_error(plot_leaflet(var = mcv2,
                            low = 1,
                            high = 2))
  expect_error(plot_leaflet(var = GDP_per_capita,
                            low = "something",
                            high = "something else"))
  expect_error(plot_leaflet(var = measles_incidence_rate_per_1000000_total_population,
                            low = "#ecf0f1",
                            high = "#f4a324",
                            format = "FALSE"))
})

test_that("var validation works", {
  expect_silent(validate_var("mcv2"))
  expect_silent(validate_var("GDP_per_capita"))
  expect_silent(validate_var("measles_incidence_rate_per_1000000_total_population"))

  expect_error(validate_var("sleep_rate"))
})

test_that("color validation works", {
  expect_true(validate_color("#ecf0f1"))
  expect_true(validate_color("#e74c3c"))
  expect_true(validate_color("forestgreen"))

  expect_error(validate_color(1))
  expect_error(validate_color("something"))
})

test_that("formatting data works", {
  mcv2_result <- formatted_data("mcv2", TRUE)
  expect_s3_class(mcv2_result, "data.frame")
  expect_equal(mcv2_result$mcv2, mcv2_result$scale_col)

  gdp_results <- formatted_data("GDP_per_capita", TRUE)
  expect_s3_class(gdp_results, "data.frame")
  expect_gte(min(gdp_results$scale_col, na.rm = TRUE), 0)
  expect_lte(max(gdp_results$scale_col, na.rm = TRUE), 1)

  measles_results <- formatted_data("measles_incidence_rate_per_1000000_total_population",
                                    FALSE)
  expect_s3_class(measles_results, "data.frame")
  expect_equal(measles_results$measles_incidence_rate_per_1000000_total_population,
               measles_results$label_col)
})

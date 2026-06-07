metadata <- load_data()

########## HELPER FUNCTION TESTS ##########
# filter_data: validating inputs and outputs of helper function.
test_that("filter_data filters by a single country", {
  result <- filter_data(metadata, c(country = "Mongolia"))
  expect_true(all(result$country == "Mongolia"))
  expect_gt(nrow(result), 0)
})

test_that("filter_data filters by a single region_s", {
  result <- filter_data(metadata, c(region_s = "AMRO"))
  expect_true(all(result$region_s == "AMRO"))
  expect_gt(nrow(result), 0)
})

test_that("filter_data applies OR logic for multiple values in the same column", {
  result <- filter_data(metadata, c(country = "Mongolia", country = "Algeria"))
  expect_setequal(unique(result$country), c("Mongolia", "Algeria"))
})

test_that("filter_data applies AND logic across different columns", {
  result <- filter_data(metadata,
                        c(region_s = "WPRO", country = "Mongolia"))
  expect_true(all(result$region_s == "WPRO"))
  expect_true(all(result$country == "Mongolia"))
})

test_that("filter_data returns a data frame", {
  result <- filter_data(metadata, c(country = "Mongolia"))
  expect_s3_class(result, "data.frame")
})

# filter_data: making sure the wrong things are in fact, wrong.

test_that("filter_data errors when data is not a data frame", {
  expect_error(filter_data("not a df", c(country = "Mongolia")),
               "`data` must be a data frame")
})

test_that("filter_data errors when filter_vector is not character", {
  expect_error(filter_data(metadata, 123),
               "`filter_vector` must be a non-empty named character vector")
})

test_that("filter_data errors when filter_vector is empty", {
  expect_error(filter_data(metadata, character(0)),
               "`filter_vector` must be a non-empty named character vector")
})

test_that("filter_data errors when filter_vector has no names", {
  expect_error(filter_data(metadata, c("Mongolia")),
               "`filter_vector` must be fully named")
})

test_that("filter_data errors when filter_vector has a partially unnamed element", {
  expect_error(filter_data(metadata, c(country = "Mongolia", "Algeria")),
               "`filter_vector` must be fully named")
})

test_that("filter_data errors on an unrecognised column name", {
  expect_error(filter_data(metadata, c(banana = "Mongolia")),
               "are not columns in the dataset")
})

test_that("filter_data errors on a value not present in the column", {
  expect_error(filter_data(metadata, c(country = "Napville")),
               "are not found in the data")
})

########## time_lapse_plot TESTS ##########

valid_param <- "mcv2"

# time_lapse_plot: output structure is correct

test_that("time_lapse_plot returns an animated (gganim) object", {
  p <- time_lapse_plot(parameters = valid_param)
  expect_s3_class(p, "gganim")
})

# time_lapse_plot: input validation 

test_that("time_lapse_plot default title contains the parameters name", {
  p <- time_lapse_plot(parameters = valid_param)
  expect_match(p$labels$title, valid_param)
})

test_that("time_lapse_plot contains a custom title_", {
  p <- time_lapse_plot(parameters = valid_param, title_ = "My Custom Title")
  expect_equal(p$labels$title, "My Custom Title")
})

# time_lapse_plot: filtering by year

test_that("time_lapse_plot data contains no years before start_year", {
  p <- time_lapse_plot(parameters = valid_param, start_year = 2015, end_year = 2020)
  expect_true(all(p$data$year >= 2015))
})

test_that("time_lapse_plot data contains no years after end_year", {
  p <- time_lapse_plot(parameters = valid_param, start_year = 2015, end_year = 2020)
  expect_true(all(p$data$year <= 2020))
})

# time_lapse_plot: data is properly filtered by filter_vector

test_that("time_lapse_plot filter_vector restricts data to one country", {
  p_filtered   <- time_lapse_plot(parameters = valid_param,
                                  filter_vector = c(country = "Mongolia"))
  p_unfiltered <- time_lapse_plot(parameters = valid_param)
  # A single-country filter produces different yearly medians than all countries
  expect_false(isTRUE(all.equal(p_filtered$data$value, p_unfiltered$data$value)))
})

test_that("time_lapse_plot filter_vector can filter by region_s", {
  p_region     <- time_lapse_plot(parameters = valid_param,
                                  filter_vector = c(region_s = "WPRO"))
  p_unfiltered <- time_lapse_plot(parameters = valid_param)
  # A single-region filter produces different yearly medians than all regions
  expect_false(isTRUE(all.equal(p_region$data$value, p_unfiltered$data$value)))
})

# time_lapse_plot: will it break

test_that("time_lapse_plot errors when parameters is not a column in the data", {
  expect_error(
    time_lapse_plot(parameters = "chicken_nuggets_per_capita"),
    "are not column names in the dataset"
  )
})

test_that("time_lapse_plot accepts a vector of parameters and plots all on one graph", {
  params <- c("mcv2", "measles_incidence_rate_per_1000000_total_population")
  p <- time_lapse_plot(parameters = params)
  expect_s3_class(p, "gganim")
  expect_setequal(unique(p$data$parameter), params)
})

test_that("time_lapse_plot errors when filter_vector names a non-existent column", {
  expect_error(
    time_lapse_plot(parameters = valid_param,
                    filter_vector = c(favorite_cuisine = "Mongolia")),
    "are not columns in the dataset"
  )
})

test_that("time_lapse_plot errors when filter_vector value is absent from the data", {
  expect_error(
    time_lapse_plot(parameters = valid_param,
                    filter_vector = c(country = "Crudbob McDoomington")),
    "are not found in the data"
  )
})

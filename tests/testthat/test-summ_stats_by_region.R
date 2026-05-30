test_that("summary statistics for a variable", {

  x <- c(1, 2, 3)

  tab <- summ_stats(x)

  # Check object type
  expect_s3_class(tab, "tibble")

  # Check expected column names
  expect_equal(names(tab), c("mean", "median", "stdev, iqr, n"))

  # Check expected values
  expect_equal(tab$mean[1], 2)
  expect_equal(tab$median[1], 2)
  expect_equal(tab$stdev[1], 1)
  expect_equal(tab$median[1], 1)
  expect_equal(tab$median[1], 3)
})

test_that("summary statistics by mode", {

  dat <- load_data()

  tab <- summarize_stats(dat)

  # Check object type
  expect_s3_class(result, "tibble")

  # Check expected column names
  expect_equal(names(tab), c("mean", "median", "stdev, iqr, n"))

  # Check expected values
  expect_equal(tab$region[1], "Africa")
  expect_equal(tab$mean[1], 261.6423)
  expect_equal(tab$median[1], 53)
  expect_equal(tab$stdev[1], 523)
  expect_equal(tab$median[1], 276)
  expect_equal(tab$median[1], 601)

})

test_that("summary statistics for a specified region (summ_stats_by_region)", {

  dat <- load_data()

  tab <- mmR::summ_stats_by_region(r = "Americas", summ_param = "mcv2")

  # Check object type
  expect_s3_class(tab, "gt")

  # Check expected column names
  expect_equal(names(tab$`_data`), c("mean", "median", "stdev, iqr, n"))

  # Check expected values
  expect_equal(tab$`_data`$country[1], "Saint Vincent and the Grenadines")
  expect_equal(tab$`_data`$mean[1], 0.99)
  expect_equal(tab$`_data`$median[1], 0.99)
  expect_equal(tab$`_data`$stdev[1], 0)
  expect_equal(tab$`_data`$iqr[1], 0)
  expect_equal(tab$`_data`$n[1], 0)

  expect_error(mmR::summ_stats_by_region(r = "z", summ_param = "mcv2"))

})

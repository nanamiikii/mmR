test_that("summary statistics for a variable (generate_summary_stats)", {

  x <- c(1, 2, 3) # numeric

  tab <- generate_summary_stats(x)

  # Check object type
  expect_s3_class(tab, "tbl_df")

  # Check expected column names
  expect_equal(names(tab), c("mean", "median", "stdev", "iqr", "n"))

  # Check expected values
  expect_equal(tab$mean[1], 2)
  expect_equal(tab$median[1], 2)
  expect_equal(tab$stdev[1], 1)
  expect_equal(tab$iqr[1], 1)
  expect_equal(tab$n[1], 3)


  tab <- generate_summary_stats(c("cats", "dogs")) # categorical

  # Check object type
  expect_s3_class(tab, "tbl_df")

  # Check expected column names
  expect_equal(names(tab), c("n_levels"))

  # Check expected values
  expect_equal(tab$n_levels[1], 2)

  # using a tibble
  expect_error(generate_summary_stats(tibble(x)))

})

test_that("summary statistics by mode (summarize_stats_by)", {

  dat <- load_data()

  tab <- summarize_stats_by(dat)

  # Check object type
  expect_s3_class(tab, "tbl_df")

  # Check expected column names
  expect_equal(names(tab), c("region", "mean", "median", "stdev", "iqr", "n"))

  # Check expected values
  expect_equal(tab$region[1], load_data()$region[1]
               #"Africa" |> as.factor()
               )
  expect_equal(tab$mean[1] |> round(), 262)
  expect_equal(tab$median[1] |> round(), 53)
  expect_equal(tab$stdev[1] |> round(), 523)
  expect_equal(tab$iqr[1] |> round(), 276)
  expect_equal(tab$n[1] |> round(), 601)

})

test_that("summary statistics for a specified region (summarize_stats_by_region)", {

  dat <- load_data()

  tab <- mmR::summarize_stats_by_region(r = "Americas", summ_param = "mcv2")

  # Check object type
  expect_s3_class(tab, "gt_tbl")

  # Check expected column names
  expect_equal(names(tab$`_data`), c("country", "mean", "median", "stdev", "iqr", "n"))

  # Check expected values
  expect_equal(tab$`_data`$country[1], "Saint Vincent and the Grenadines")
  expect_equal(tab$`_data`$mean[1], 0.99)
  expect_equal(tab$`_data`$median[1], 0.99)
  expect_equal(tab$`_data`$stdev[1], 0)
  expect_equal(tab$`_data`$iqr[1], 0)
  expect_equal(tab$`_data`$n[1], 3)

  expect_error(mmR::summarize_stats_by_region(r = "z", summ_param = "mcv2"))
  expect_error(mmR::summarize_stats_by_region(r = "Americas", summ_param = "z"))

  dat <- load_data()

  tab <- mmR::summarize_stats_by_region()

  expect_s3_class(tab, "gt_tbl")

  expect_equal(names(tab$`_data`), c("country", "mean", "median", "stdev", "iqr", "n"))

  expect_equal(tab$`_data`$country[1], "Madagascar")
  expect_equal(tab$`_data`$mean[1], 16,139.07)
  expect_equal(tab$`_data`$median[1], 72.50)
  expect_equal(tab$`_data`$stdev[1], 56,834.29)
  expect_equal(tab$`_data`$iqr[1], 78.25)
  expect_equal(tab$`_data`$n[1], 14)

})

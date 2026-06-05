test_that("model forming function works", {

  expect_s3_class(model_form(), "gls")

  expect_error(model_form("z"))

})

test_that("model table function works", {

  tab <- model_tab()

  expect_s3_class(tab, "gt_tbl")
  expect_equal(names(tab$`_data`), c("var",
                                     "Value",
                                     "Std.Error",
                                     "t-value",
                                     "p-value",
                                     "region"
                                     )
               )
  expect_equal(tab$`_data`$Value[1] |> round(), 271)
  expect_equal(tab$`_data`$Std.Error[1] |> round(), 69)
  expect_equal(tab$`_data`$`t-value`[1] |> round(), 4)
  expect_equal(tab$`_data`$`p-value`[1] |> round(), 0)
  expect_equal(tab$`_data`$region[1], "Quantitative")

  expect_error(model_tab("z"))

})

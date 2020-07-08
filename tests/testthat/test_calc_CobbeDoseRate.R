context("calc_CobbleDoseRate")

test_that("basic checks", {
  testthat::skip_on_cran()

  ## simple run with example data
  data("ExampleData.CobbleData", envir = environment())
  expect_s4_class(calc_CobbleDoseRate(ExampleData.CobbleData), "RLum.Results")

  ## break the function
  df <- ExampleData.CobbleData
  df$Distance[[14]] <- 50000
  expect_error(calc_CobbleDoseRate(df),
               "Slices outside of cobble. Please check your distances and make sure they are in mm and diameter is in cm!")
})

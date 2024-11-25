## load data
data(ExampleData.CobbleData, envir = environment())
df <- ExampleData.CobbleData

test_that("input validation", {
  testthat::skip_on_cran()

  df$Distance[[14]] <- 50000
  expect_error(calc_CobbleDoseRate("error"),
               "'input' should be of class 'data.frame'")
  expect_error(calc_CobbleDoseRate(data.frame()),
               "'input' cannot be an empty data.frame")
  expect_error(calc_CobbleDoseRate(df),
               "Slices outside of cobble: please ensure your distances are in mm")

  expect_error(calc_CobbleDoseRate(ExampleData.CobbleData, conversion = "error"),
               "'conversion' should be one of 'Guerinetal2011', 'Cresswelletal2018'")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6

  expect_snapshot_RLum(calc_CobbleDoseRate(df),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(calc_CobbleDoseRate(df, conv = "Cresswelletal2018"),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(calc_CobbleDoseRate(df, conv = "Liritzisetal2013"),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(calc_CobbleDoseRate(df, conv = "AdamiecAitken1998"),
                       tolerance = snapshot.tolerance)
})

test_that("more coverage", {
  testthat::skip_on_cran()

  df$CobbleDiameter[1] <- 25
  expect_s4_class(calc_CobbleDoseRate(df),
                  "RLum.Results")

  df$DistanceError[1] <- 0
  df$CobbleDiameter[1] <- 2
  df$Thickness <- 10
  df <- df[1:9, ]
  expect_s4_class(calc_CobbleDoseRate(df),
                  "RLum.Results")
})

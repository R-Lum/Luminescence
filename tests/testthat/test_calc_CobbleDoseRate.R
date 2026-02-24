## load data
data(ExampleData.CobbleData, envir = environment())
df <- ExampleData.CobbleData

test_that("input validation", {
  testthat::skip_on_cran()

  df$Distance[[14]] <- 50000
  expect_error(calc_CobbleDoseRate("error"),
               "'object' should be of class 'data.frame'")
  expect_error(calc_CobbleDoseRate(data.frame()),
               "'object' cannot be an empty data.frame")
  expect_error(calc_CobbleDoseRate(iris),
               "'object' doesn't contain the following columns:")
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

  ## deprecated argument
  expect_warning(calc_CobbleDoseRate(input = df),
                 "'input' was deprecated in v1.2.0, use 'object' instead")
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 1424
  df <- data.frame(Distance = 0, DistanceError = 0.01, Thickness = 0.6,
                   ThicknessError = 0.01, Mineral = "Q", Cobble_K = 0.008,
                   Cobble_K_SE = 0.0008, Cobble_Th = 0.62, Cobble_Th_SE = 0.06,
                   Cobble_U = 0.2, Cobble_U_SE = 0.02, GrainSize = 9000,
                   Density = 2.7, CobbleDiameter = 2.6, Sed_K = 0.09,
                   Sed_K_SE = 0.009, Sed_Th = 2.4, Sed_Th_SE = 0.2,
                   Sed_U = 0.75, Sed_U_SE = 0.07, GrainSize_Sed = 200L,
                   Density_Sed = 1.8, WaterContent = 8.2, WaterContent_SE = 4.1)
  expect_error(calc_CobbleDoseRate(df, conversion = "Cresswelletal2018"),
               "No attenuation data available for the grain size provided (9000)",
               fixed = TRUE)
})

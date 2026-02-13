## load data
data(ExampleData.CW_OSL_Curve, envir = environment())
curve <- set_RLum(class = "RLum.Data.Curve",
                  recordType = "OSL",
                  data = as.matrix(ExampleData.CW_OSL_Curve))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(bin_RLum.Data(set_RLum("RLum.Data.Spectrum")),
               "'object' contains no data")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6

  expect_snapshot_RLum(bin_RLum.Data(curve),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(bin_RLum.Data(curve, bin = 5),
                       tolerance = snapshot.tolerance)
})

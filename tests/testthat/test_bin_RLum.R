## load data
data(ExampleData.CW_OSL_Curve, envir = environment())
curve <- set_RLum(class = "RLum.Data.Curve",
                  recordType = "OSL",
                  data = as.matrix(ExampleData.CW_OSL_Curve))
spectrum <- set_RLum(class = "RLum.Data.Spectrum",
                     data = matrix(rep(1:20, each = 10), ncol = 20,
                                   dimnames = list(1:10, 1:20)))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(bin_RLum(curve, bin_size = -2),
               "'bin_size' should be a single positive integer value")
  expect_error(bin_RLum(spectrum, bin_size.row = "test"),
               "'bin_size.row' should be a single positive integer value")
  expect_error(bin_RLum(spectrum, bin_size.row = 12, bin_size.col = "test"),
               "'bin_size.col' should be a single positive integer value")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_silent(bin_RLum(set_RLum("RLum.Data.Curve")))
  expect_silent(bin_RLum(set_RLum("RLum.Data.Spectrum")))
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6

  expect_snapshot_RLum(bin_RLum(curve),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(bin_RLum(curve, bin_size = 5),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(bin_RLum(spectrum, bin_size.row = 2),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(bin_RLum(spectrum, bin_size.row = 1, bin_size.col = 2),
                       tolerance = snapshot.tolerance)
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 1798
  expect_silent(res <- bin_RLum(set_RLum(class = "RLum.Data.Spectrum",
                                         data = matrix(data = 1:16, ncol = 4)),
                                bin_size.col = 2))
  expect_equal(colnames(res@data),
               as.character(c(2, 4)))
  expect_equal(rownames(res@data),
               as.character(1:4))
})

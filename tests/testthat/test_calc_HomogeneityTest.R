##use the data example given by Galbraith (2003)
df <- data.frame(
    x = c(30.1, 53.8, 54.3, 29.0, 47.6, 44.2, 43.1),
    y = c(4.8, 7.1, 6.8, 4.3, 5.2, 5.9, 3.0))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_HomogeneityTest(TRUE),
               "'data' should be of class 'data.frame' or 'RLum.Results'")
  expect_error(calc_HomogeneityTest(data.frame()),
               "'data' cannot be an empty data.frame")
  expect_error(calc_HomogeneityTest(iris[, 1, drop = FALSE]),
               "'data' should have 2 columns")
  expect_error(calc_HomogeneityTest(data.frame(1:4, letters[1:4])),
               "All columns of 'data' should be of class 'numeric' or 'integer'")
})

test_that("check values from output example", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6

  expect_snapshot_RLum(temp <- calc_HomogeneityTest(df, verbose = FALSE),
                       tolerance = snapshot.tolerance)

  ## using an RLum.Results object as input
  SW({
  expect_s4_class(calc_HomogeneityTest(temp),
                  "RLum.Results")
  })

  ##test the unlogged version
  SW({
  expect_snapshot_RLum(calc_HomogeneityTest(df, log = FALSE),
                       tolerance = snapshot.tolerance)
  })

  ## negative values in data
  expect_warning(expect_warning(
      calc_HomogeneityTest(data.frame(1:5, -1:3), verbose = FALSE),
      "'data' contains negative values and 'log = TRUE'"),
      "NaNs produced")
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 924
  expect_s4_class(
      calc_HomogeneityTest(data.frame(1:4, NA_real_), verbose = FALSE),
      "RLum.Results")

  ## issue 1224
  expect_silent(calc_HomogeneityTest(data.frame(1:4, 2:5, letters[1:4]),
                                     verbose = FALSE))
})

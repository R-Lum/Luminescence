## load data
data(ExampleData.DeValues, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  df <- ExampleData.DeValues$CA1
  expect_error(calc_IEU("error", a = 0.2, b = 1.9, interval = 1),
               "'data' should be of class 'data.frame' or 'RLum.Results'")
  expect_error(calc_IEU(df[, 1, drop = FALSE], a = 0.2, b = 1.9, interval = 1),
               "'data' should have at least two columns")
  expect_error(calc_IEU(df, a = "error", b = 1.9, interval = 1),
               "'a' should be of class 'numeric'")
  expect_error(calc_IEU(df, a = 0.2, b = "error", interval = 1),
               "'b' should be of class 'numeric'")
  expect_error(calc_IEU(df, a = 0.2, b = 1.9, interval = "error"),
               "'interval' should be of class 'numeric'")
  expect_error(calc_IEU(data.frame(), a = 0.2, b = 1.9, interval = 1),
               "'data' contains no data")
  expect_error(calc_IEU(iris[0, ], a = 0.2, b = 1.9, interval = 1),
               "'data' contains no data")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ##standard
  expect_silent(calc_IEU(
    ExampleData.DeValues$CA1,
    a = 0.2,
    b = 1.9,
    interval = 1,
    verbose = FALSE, plot =FALSE
  ))

  ## enable plot and verbose (using default values for coverage)
  SW({
  expect_message(calc_IEU(
    ExampleData.DeValues$CA1,
    a = 0.2,
    b = 1.9,
    interval = 1,
    trace = TRUE
  ))
  })
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6

  expect_snapshot_RLum(calc_IEU(
      ExampleData.DeValues$CA1,
      a = 0.25,
      b = 1.29,
      interval = 1,
      verbose = FALSE,
      plot = FALSE),
      tolerance = snapshot.tolerance)

  ##provide RLum.Results
  expect_snapshot_RLum(calc_IEU(
      set_RLum(class = "RLum.Results",
               data = list(test = ExampleData.DeValues$CA1)),
      a = 0.12,
      b = 2.14,
      interval = 0.9,
      verbose = FALSE, plot = FALSE),
      tolerance = snapshot.tolerance)
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 424
  expect_warning(calc_IEU(
      ExampleData.DeValues$CA1,
      a = 0.45,
      b = 1.29,
      interval = 1,
      verbose = FALSE,
      plot = FALSE),
      "Numerical error, try changing your 'a' and 'b' values")
  expect_warning(calc_IEU(
      ExampleData.DeValues$CA1,
      a = 0.12,
      b = 1.29,
      interval = 10,
      verbose = FALSE,
      plot = FALSE),
      "Numerical error, try changing your 'a' and 'b' values")
})

test_that("test additional C++ functionality", {
  testthat::skip_on_cran()

  SW({
  expect_type(Luminescence:::src_EED_Calc_Overall_StatUncertainty(
    M_Simul = matrix(1:100, ncol = 20),
    Ndata = 20,
    Nsimul = 2,
    MinNbSimExp = 20), "double")
  })
})

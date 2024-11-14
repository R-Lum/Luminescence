## load data
data(ExampleData.DeValues, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_IEU("error", a = 0.2, b = 1.9, interval = 1),
               "'data' should be of class 'data.frame' or 'RLum.Results'")
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

  ##enable plot
  SW({
  expect_message(calc_IEU(
    ExampleData.DeValues$CA1,
    a = 0.2,
    b = 1.9,
    interval = 1,
    trace = TRUE,
    verbose = TRUE, plot = TRUE
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

## load data
data(ExampleData.DeValues, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_warning(expect_null(calc_WodaFuchs2008("error")),
                 "'data' should be of class 'data.frame', 'RLum.Results' or")
  expect_error(calc_WodaFuchs2008(data.frame()),
               "'data' cannot be an empty data.frame")
  expect_error(calc_WodaFuchs2008(data.frame(a = letters)),
               "'data' should have only numeric fields")
  expect_error(calc_WodaFuchs2008(ExampleData.DeValues$CA1, breaks = 0),
               "'breaks' should be a single positive value")
  res <- calc_WodaFuchs2008(ExampleData.DeValues$CA1)
  expect_error(calc_WodaFuchs2008(res, breaks = 4),
               "Insufficient number of data points")
  expect_error(expect_message(
      calc_WodaFuchs2008(c(-1, 0, 1)),
      "No errors provided, bin width set by 10 percent of input data"),
      "The estimated bin width is not positive, check your data")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ##test arguments
  expect_s4_class(calc_WodaFuchs2008(ExampleData.DeValues$CA1, plot = FALSE),
                  "RLum.Results")
  expect_warning(calc_WodaFuchs2008(ExampleData.DeValues$CA1[1:40, ]),
                 "More than one maximum, fit may be invalid")

  ## issue 197
  set.seed(1)
  df <- data.frame(rnorm(20, 10), rnorm(20, 0.5))
  expect_silent(calc_WodaFuchs2008(df))

  ## more coverage
  expect_warning(calc_WodaFuchs2008(df, breaks = 3),
                 "Fewer than 4 bins produced, 'breaks' set to 4")
  expect_output(calc_WodaFuchs2008(df, trace = TRUE))

  ## numeric vector
  expect_message(calc_WodaFuchs2008(df[, 1]),
                 "No errors provided")

  ## single-column data.frame
  expect_message(calc_WodaFuchs2008(df[, 1, drop = FALSE]),
                 "No errors provided")

  ## RLum.Results object
  obj <- calc_CommonDose(ExampleData.DeValues$BT998, verbose = FALSE)
  expect_silent(calc_WodaFuchs2008(obj))

  ## Inf values
  expect_warning(expect_warning(
      calc_WodaFuchs2008(data.frame(c(Inf, -0.5, 1234, 5), rep(1, 4))),
      "Inf values found in 'data', replaced by NA"),
      "More than one maximum, fit may be invalid")
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  SW({
  vdiffr::expect_doppelganger("defaults",
                              calc_WodaFuchs2008(ExampleData.DeValues$CA1))
  vdiffr::expect_doppelganger("main breaks",
                              calc_WodaFuchs2008(ExampleData.DeValues$CA1,
                                                 main = "Woda-Fuchs (2008)",
                                                 breaks = 20))
  })
})

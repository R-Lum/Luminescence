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
  res <- calc_WodaFuchs2008(ExampleData.DeValues$CA1)
  expect_error(calc_WodaFuchs2008(res, breaks = 4),
               "Insufficient number of data points")
  expect_error(expect_message(
      calc_WodaFuchs2008(data.frame(c(-1, 0, 1))),
      "No errors provided, bin width set by 10 percent of input data"),
      "The estimated bin width is not positive, check your data")
})

test_that("Test general functionality", {
  testthat::skip_on_cran()

  ##test arguments
  expect_s4_class(calc_WodaFuchs2008(ExampleData.DeValues$CA1),
                  "RLum.Results")
  expect_s4_class(calc_WodaFuchs2008(ExampleData.DeValues$CA1, plot = FALSE),
                  "RLum.Results")
  expect_s4_class(calc_WodaFuchs2008(ExampleData.DeValues$CA1, breaks = 20),
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
})

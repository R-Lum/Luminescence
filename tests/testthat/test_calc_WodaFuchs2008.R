data(ExampleData.DeValues, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_warning(expect_null(calc_WodaFuchs2008("error")),
                 "'data' should be of class 'data.frame', 'RLum.Results' or")
  expect_error(calc_WodaFuchs2008(data.frame()),
               "'data' cannot be an empty data.frame")
  res <- calc_WodaFuchs2008(ExampleData.DeValues$CA1)
  expect_error(calc_WodaFuchs2008(res, breaks = 4),
               "Insufficient number of data points")
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

  ## issue #197
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

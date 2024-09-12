data(ExampleData.DeValues, envir = environment())
results <- Second2Gray(ExampleData.DeValues$BT998, c(0.2,0.01))
results_alt1 <- Second2Gray(ExampleData.DeValues$BT998, c(0.2,0.01), error.propagation = "gaussian")
results_alt2 <- Second2Gray(ExampleData.DeValues$BT998, c(0.2,0.01), error.propagation = "absolute")
dose.rate <- calc_SourceDoseRate(calib.date = "2014-12-19",
                                 calib.dose.rate = 0.2, calib.error = 0.01)
results_alt3 <- Second2Gray(ExampleData.DeValues$BT998, dose.rate = dose.rate)

Second2Gray(ExampleData.DeValues$BT998, results, error.prop = "gaussian")
Second2Gray(ExampleData.DeValues$BT998, results, error.prop = "absolute")


test_that("check class and length of output", {
  testthat::skip_on_cran()

  expect_s3_class(results, class = "data.frame")

  expect_error(Second2Gray("test"),
               "'data' object has to be of type 'data.frame'")
  expect_error(Second2Gray(ExampleData.DeValues$BT998, dose.rate = FALSE),
               "'dose.rate' object has to be of type")
  expect_error(Second2Gray(ExampleData.DeValues$BT998,
                           dose.rate = results[1:5, ]),
               "'data' and 'dose.rate' need to be of similar length")
  expect_error(Second2Gray(ExampleData.DeValues$BT998,
                           dose.rate = results,
                           error.propagation = "test"),
               "unsupported error propagation method")
  dose.rate@originator <- "unexpected-originator"
  expect_error(Second2Gray(ExampleData.DeValues$BT998, dose.rate = dose.rate),
               "Wrong originator for dose.rate 'RLum.Results' object")
})

test_that("check values from output example", {
  testthat::skip_on_cran()

  expect_equal(sum(results[[1]]), 14754.09)
  expect_equal(sum(results[[2]]), 507.692)
  expect_equal(sum(results_alt1[[2]]), 895.911)
  expect_equal(sum(results_alt2[[2]]), 1245.398)

})

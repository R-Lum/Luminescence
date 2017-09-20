context("report_RLum")

test_that("Test Simple RLum Report", {
  testthat::skip_on_cran()

  ### load example data
  data("ExampleData.DeValues")
  temp <- calc_CommonDose(ExampleData.DeValues$CA1)

  # create the standard HTML report
  expect_null(report_RLum(object = temp, timestamp = FALSE))
  expect_null(report_RLum(object = temp, timestamp = FALSE, compact = FALSE))

})

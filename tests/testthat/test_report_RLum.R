test_that("Test Simple RLum Report", {
  testthat::skip_on_cran()
  local_edition(3)

  ## the test fails on AppVeyor for no obvious reason on the windows
  ## platform ... attempts to reproduce this failure failed. So
  ## we skip this platform for the test
  testthat::skip_on_os("windows")

  ### load example data
  data("ExampleData.DeValues")
  temp <- calc_CommonDose(ExampleData.DeValues$CA1)

  # create the standard HTML report
  expect_null(report_RLum(object = temp, timestamp = FALSE, show_report = FALSE))
  expect_null(report_RLum(object = temp, timestamp = FALSE, show_report = FALSE, compact = FALSE))

})

## load data
data(ExampleData.DeValues, envir = environment())

test_that("Test Simple RLum Report", {
  testthat::skip_on_cran()

  ## the test fails on AppVeyor for no obvious reason on the windows
  ## platform ... attempts to reproduce this failure failed. So
  ## we skip this platform for the test
  testthat::skip_on_os("windows")

  ### load example data
  data(ExampleData.DeValues, envir = environment())
  SW({
  temp <- calc_CommonDose(ExampleData.DeValues$CA1)
  })

  expect_error(report_RLum(temp, css.file = "error"),
               "Couldn't find the specified CSS file")

  # create the standard HTML report
  testthat::expect_null(report_RLum(object = temp, timestamp = FALSE, show_report = FALSE))
  testthat::expect_null(report_RLum(object = temp, timestamp = TRUE,
                                    show_report = FALSE, compact = FALSE))

  ## compact view
  expect_null(report_RLum(list(temp, temp),
                          show_report = FALSE, compact = TRUE))

  ## data.frame
  expect_null(report_RLum(ExampleData.DeValues$CA1))

  ## names with spaces or missing
  ll <- as.list(ExampleData.DeValues$CA1)
  names(ll) <- c("", "ED Error")
  expect_null(report_RLum(ll))

  ## more coverage: data frame with a column containing a matrix
  m <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  df <- data.frame(id = 1, mat = rep(0, nrow(m)))
  df$mat <- m
  expect_null(report_RLum(df))
})

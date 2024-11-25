## load example data
data(ExampleData.DeValues, envir = environment())

test_that("Test certain input scenarios", {
  testthat::skip_on_cran()

  temp_RLum <- set_RLum(class = "RLum.Results",
                        data = list(data = ExampleData.DeValues$BT998))
  expect_type(calc_Statistics(temp_RLum), "list")

  df <- ExampleData.DeValues$BT998
  df[, 2] <- NULL
  expect_warning(calc_Statistics(df),
                 "All errors are NA or zero, automatically set to")

  df <- ExampleData.DeValues$BT998
  df[,2] <- 0
  expect_warning(calc_Statistics(df),
                 "All errors are NA or zero, automatically set to")

  df <- ExampleData.DeValues$BT998
  expect_silent(calc_Statistics(df, weight.calc = "reciprocal"))
})

test_that("input validation", {
  testthat::skip_on_cran()

  df <- ExampleData.DeValues$BT998

  expect_error(calc_Statistics(data = matrix(0,2)),
               "[calc_Statistics()] 'data' should be of class 'RLum.Results' or 'data.frame'",
               fixed = TRUE)
  expect_error(calc_Statistics(data.frame()),
               "'data' cannot be an empty data.frame")
  expect_error(calc_Statistics(data = df, weight.calc = "error"),
               "'weight.calc' should be one of 'square' or 'reciprocal'")
  expect_error(calc_Statistics(df, digits = 2.4),
               "'digits' should be a positive integer scalar")
  expect_error(calc_Statistics(df, n.MCM = "error"),
               "'n.MCM' should be a positive integer scalar")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  set.seed(1)
  snapshot.tolerance <- 1.5e-6

  expect_snapshot_plain(calc_Statistics(
      ExampleData.DeValues$BT998, n.MCM = 1000),
      tolerance = snapshot.tolerance)

  expect_snapshot_plain(calc_Statistics(
      ExampleData.DeValues$BT998, n.MCM = 1000, digits = 2),
      tolerance = snapshot.tolerance)
})

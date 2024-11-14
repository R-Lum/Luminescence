## load data
data(ExampleData.DeValues, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_CommonDose(),
               "'data' should be of class 'data.frame' or 'RLum.Results'")
  expect_error(calc_CommonDose("test"),
               "'data' should be of class 'data.frame' or 'RLum.Results'")
  expect_error(calc_CommonDose(data.frame(col = 1:10)),
               "'data' object must have two columns")
  expect_error(calc_CommonDose(data.frame(col = 1:10)),
               "'data' object must have two columns")
  expect_error(calc_CommonDose(ExampleData.DeValues$CA1, sigmab = 2),
               "'sigmab' must be a value between 0 and 1")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  SW({
    temp <- calc_CommonDose(ExampleData.DeValues$CA1, plot = FALSE,
                            verbose = TRUE)
    temp.nolog <- calc_CommonDose(ExampleData.DeValues$CA1, log = FALSE,
                                  plot = FALSE, verbose = TRUE)
  })
  expect_s4_class(temp, "RLum.Results")
  expect_equal(length(temp), 4)

  all.equal(calc_CommonDose(temp, verbose = FALSE),
            temp)

  results <- get_RLum(temp)
  expect_equal(round(results$de, digits = 5), 62.15999)
  expect_equal(round(results$de_err, digits = 7), 0.7815117)
  expect_true(temp@data$args$log)
  expect_equal(temp@data$args$sigmab, 0)

  results <- get_RLum(temp.nolog)
  expect_false(temp.nolog@data$args$log)
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6

  expect_snapshot_RLum(calc_CommonDose(
      ExampleData.DeValues$CA1, sigmab = 0.75,
      plot = FALSE, verbose = FALSE),
      tolerance = snapshot.tolerance)
  expect_snapshot_RLum(calc_CommonDose(
      ExampleData.DeValues$CA1, sigmab = 0.25, log = FALSE,
      plot = FALSE, verbose = FALSE),
      tolerance = snapshot.tolerance)
})

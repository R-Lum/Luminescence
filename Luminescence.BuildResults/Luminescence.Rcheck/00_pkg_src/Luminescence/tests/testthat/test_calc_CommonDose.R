data(ExampleData.DeValues, envir = environment())
SW({
temp <- calc_CommonDose(ExampleData.DeValues$CA1, plot = FALSE, verbose = TRUE)
temp.nolog <- calc_CommonDose(ExampleData.DeValues$CA1, log = FALSE,
                              plot = FALSE, verbose = TRUE)
})

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_CommonDose(),
               "is missing, with no default")
  expect_error(calc_CommonDose("test"),
               "object has to be of type 'data.frame' or 'RLum.Results'")
  expect_error(calc_CommonDose(data.frame(col = 1:10)),
               "'data' object must have two columns")
  expect_error(calc_CommonDose(data.frame(col = 1:10)),
               "'data' object must have two columns")
  expect_error(calc_CommonDose(ExampleData.DeValues$CA1, sigmab = 2),
               "'sigmab' must be a value between 0 and 1")
})

test_that("check class and length of output", {
  testthat::skip_on_cran()

  expect_s4_class(temp, "RLum.Results")
  expect_equal(length(temp), 4)

})

test_that("check values from output", {
  testthat::skip_on_cran()

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

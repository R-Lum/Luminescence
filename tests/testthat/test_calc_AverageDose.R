data(ExampleData.DeValues, envir = environment())
temp <- calc_AverageDose(ExampleData.DeValues$CA1[1:56,],
                       sigma_m = 0.1,
                       plot = FALSE,
                       verbose = FALSE)

test_that("input validation", {
  testthat::skip_on_cran()

  data <- ExampleData.DeValues$CA1
  expect_error(calc_AverageDose(),
               "is missing, with no default")
  expect_error(calc_AverageDose("test"),
               "Input must be of type 'RLum.Results' or 'data.frame'")
  expect_error(calc_AverageDose(data),
               "\"sigma_m\" is missing, with no default")
  expect_error(calc_AverageDose(data, sigma_m = NULL),
               "'sigma_m' must be a positive scalar")
  expect_error(calc_AverageDose(data, sigma_m = 0.1, Nb_BE = NULL),
               "'Nb_BE' must be a positive integer scalar")
  expect_message(expect_null(
      calc_AverageDose(data[, 1, drop = FALSE], sigma_m = 0.1)),
      "Error: data set contains < 2 columns")
  expect_message(expect_null(
      calc_AverageDose(data[0, ], sigma_m = 0.1)),
      "Error: data set contains 0 rows")

  SW({
  expect_warning(calc_AverageDose(cbind(data, data), sigma_m = 0.1),
                 "number of columns in data set > 2")
  expect_warning(calc_AverageDose(rbind(data, NA), sigma_m = 0.1),
                 "NA values in data set detected")
  })
})

test_that("check class and length of output", {
  testthat::skip_on_cran()

  expect_s4_class(temp, "RLum.Results")
  expect_equal(length(temp), 3)

})

test_that("check summary output", {
  testthat::skip_on_cran()

  results <- get_RLum(temp)

  expect_equal(round(results$AVERAGE_DOSE, digits = 4), 65.3597)
  expect_equal(round(results$SIGMA_D, digits = 4), 0.3092)
  expect_equal(round(results$L_MAX, digits = 5), -19.25096)
})

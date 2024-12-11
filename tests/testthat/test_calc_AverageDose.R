## load data
data(ExampleData.DeValues, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  data <- ExampleData.DeValues$CA1
  expect_error(calc_AverageDose(),
               "'data' should be of class 'RLum.Results' or 'data.frame'")
  expect_error(calc_AverageDose("test"),
               "'data' should be of class 'RLum.Results' or 'data.frame'")
  expect_error(calc_AverageDose(data),
               "\"sigma_m\" is missing, with no default")
  expect_error(calc_AverageDose(data, sigma_m = NULL),
               "'sigma_m' should be a positive scalar")
  expect_error(calc_AverageDose(data, sigma_m = 0.1, Nb_BE = NULL),
               "'Nb_BE' should be a positive integer scalar")
  expect_message(expect_null(
      calc_AverageDose(data[, 1, drop = FALSE], sigma_m = 0.1)),
      "Error: 'data' contains < 2 columns")
  expect_message(expect_null(
      calc_AverageDose(data[0, ], sigma_m = 0.1)),
      "Error: 'data' contains no data, NULL returned")

  SW({
  expect_warning(calc_AverageDose(cbind(data, data), sigma_m = 0.1),
                 "'data' contains > 2 columns")
  expect_warning(calc_AverageDose(rbind(data, NA), sigma_m = 0.1),
                 "NA values in 'data' detected")
  expect_message(expect_null(
      calc_AverageDose(data.frame(NA, NA), sigma_m = 0.1)),
      "Error: After NA removal, nothing is left from the data set")
  })
})

test_that("check class and length of output", {
  testthat::skip_on_cran()

  set.seed(1)
  expect_snapshot_RLum(
      temp <- calc_AverageDose(ExampleData.DeValues$CA1[1:56, ],
                               sigma_m = 0.1,
                               plot = FALSE,
                               verbose = FALSE),
      tolerance = 1.5e-6)
  expect_output(
      calc_AverageDose(ExampleData.DeValues$CA1[1:56, ],
                       sigma_m = 0.9,
                       plot = FALSE,
                       verbose = TRUE)
  )

  expect_s4_class(temp, "RLum.Results")
  expect_equal(length(temp), 3)

  results <- get_RLum(temp)

  expect_equal(round(results$AVERAGE_DOSE, digits = 4), 65.3597)
  expect_equal(round(results$SIGMA_D, digits = 4), 0.3092)
  expect_equal(round(results$L_MAX, digits = 5), -19.25096)

  ## RLum.Results
  expect_warning(calc_AverageDose(temp, sigma_m = 0.1, verbose = FALSE, col = 1),
                 "'data' contains > 2 columns, only the first 2 columns were used")

  ## non-positive values
  data.zero <- ExampleData.DeValues$CA1
  data.zero[1, 1] <- 0
  expect_warning(calc_AverageDose(data.zero, sigma_m = 0.1, verbose = FALSE),
                 "Non-positive values in 'data' detected, rows removed")
})

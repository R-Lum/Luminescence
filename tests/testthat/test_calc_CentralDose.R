data(ExampleData.DeValues, envir = environment())

temp <- calc_CentralDose(
  ExampleData.DeValues$CA1,
  plot = FALSE,
  verbose = FALSE)

set.seed(1)
temp_NA <- data.frame(rnorm(10)+5, rnorm(10)+5)
temp_NA[1,1] <- NA

test_that("errors and warnings function", {
  testthat::skip_on_cran()

  expect_error(calc_CentralDose(data = "error"),
               "'data' should be of class 'data.frame' or 'RLum.Results'")
  expect_error(calc_CentralDose(temp, sigmab = 10),
               "'sigmab' should be a fraction between 0 and 1")
  expect_error(calc_CentralDose(data.frame()),
               "should have at least two columns and two rows")

  SW({
  expect_s4_class(calc_CentralDose(temp_NA), "RLum.Results")
  expect_warning(calc_CentralDose(temp, na.rm = FALSE),
                 "'na.rm' is deprecated, missing values are always")
  expect_message(calc_CentralDose(temp_NA),
                 "NA values removed from dataset")
  })
})

test_that("check functionality", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6
  expect_snapshot_RLum(temp,
                       tolerance = snapshot.tolerance)
  SW({
  expect_snapshot_RLum(calc_CentralDose(ExampleData.DeValues$CA1,
                                        log = FALSE, trace = TRUE),
                       tolerance = snapshot.tolerance)

  expect_snapshot_RLum(calc_CentralDose(temp_NA, log = FALSE),
                       tolerance = snapshot.tolerance)

  ## negative De values
  neg_vals <- data.frame(De = c(-0.56, -0.16, 0.0, 0.04),
                         De.err = c(0.15, 0.1, 0.12, 0.1))
  expect_warning(calc_CentralDose(neg_vals, log = TRUE),
                 "'data' contains non-positive De values, 'log' set to FALSE")

  ## negative De errors
  neg_errs <- data.frame(De = c(0.56, 0.16, 0.1, 0.04),
                         De.err = c(-0.15, -0.1, 0.12, -0.1))
  res1 <- calc_CentralDose(neg_errs)
  res2 <- calc_CentralDose(abs(neg_errs))
  res1@info$call <- res2@info$call <- NULL
  res1@.uid <- res2@.uid <- NA_character_
  expect_equal(res1, res2)
  })
})

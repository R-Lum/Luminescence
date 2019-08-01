context("calc_CentralDose")

data(ExampleData.DeValues, envir = environment())

temp <- calc_CentralDose(
  ExampleData.DeValues$CA1,
  plot = FALSE,
  verbose = FALSE)

temp_NA <- data.frame(rnorm(10)+5, rnorm(10)+5)
temp_NA[1,1] <- NA

test_that("errors and warnings function", {
  testthat::skip_on_cran()

  expect_error(calc_CentralDose(data = "error"), "'data' has to be of type 'data.frame' or 'RLum.Results'!")
  expect_error(calc_CentralDose(temp, sigmab = 10), "sigmab needs to be given as a fraction between 0 and 1")
  expect_s4_class(calc_CentralDose(temp_NA), "RLum.Results")
  expect_warning(calc_CentralDose(temp_NA, na.rm = TRUE))
  expect_error(calc_CentralDose(data.frame()), "should have at least two columns and two rows!")

})


test_that("standard and output", {
  testthat::skip_on_cran()

  expect_equal(is(temp), c("RLum.Results", "RLum"))
  expect_equal(length(temp), 4)

  ##log and trace
  expect_s4_class(calc_CentralDose(ExampleData.DeValues$CA1, log = FALSE, trace = TRUE), "RLum.Results")




})

test_that("check summary output", {
  testthat::skip_on_cran()
  results <- get_RLum(temp)

  expect_equal(round(results$de, digits = 5), 65.70929)
  expect_equal(round(results$de_err, digits = 6), 3.053443)
  expect_equal(round(results$OD, digits = 5), 34.69061)
  expect_equal(round(results$OD_err, digits = 6), 3.458774)
  expect_equal(round(results$Lmax, digits = 5), 31.85046)
})


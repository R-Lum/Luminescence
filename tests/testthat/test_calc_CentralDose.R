context("calc_CentralDose")

data(ExampleData.DeValues, envir = environment())
temp <- calc_CentralDose(ExampleData.DeValues$CA1, plot = FALSE)


test_that("check class and length of output", {
  
  expect_equal(is(temp), c("RLum.Results", "RLum"))
  expect_equal(length(temp), 5)
  
})

test_that("check values from output", {
  
   
  results <- get_RLum(temp)
  
  expect_equal(round(results$de, digits = 5), 65.70929)
  expect_equal(round(results$de_err, digits = 6), 3.053443)
  expect_equal(round(results$OD, digits = 5), 34.69061)
  expect_equal(round(results$OD_err, digits = 6), 3.458774)
  expect_equal(round(results$Lmax, digits = 5), 31.85046)
  expect_equal(temp@data$args$log, "TRUE")
  expect_equal(temp@data$args$sigmab, 0)

})

test_that("check values from output with sigmab", {
  
  temp <- calc_CentralDose(ExampleData.DeValues$CA1, sigmab = 0.2)
  
  expect_equal(temp@data$args$sigmab, 0.2)
})
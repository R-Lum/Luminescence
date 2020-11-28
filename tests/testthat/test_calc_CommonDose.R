data(ExampleData.DeValues, envir = environment())
temp <- calc_CommonDose(ExampleData.DeValues$CA1, plot = FALSE, verbose = FALSE)

test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_s4_class(temp, "RLum.Results")
  expect_equal(length(temp), 4)

})

test_that("check values from output", {
  testthat::skip_on_cran()
  local_edition(3)

  results <- get_RLum(temp)

  expect_equal(round(results$de, digits = 5), 62.15999)
  expect_equal(round(results$de_err, digits = 7), 0.7815117)

  expect_true(temp@data$args$log)
  expect_equal(temp@data$args$sigmab, 0)


})

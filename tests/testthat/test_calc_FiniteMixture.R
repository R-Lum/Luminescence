test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

  ## load example data
  data(ExampleData.DeValues, envir = environment())

  ## simple run
  temp <- expect_s4_class(calc_FiniteMixture(
    ExampleData.DeValues$CA1,
    sigmab = 0.2,
    n.components = 2,
    grain.probability = TRUE,
    verbose = TRUE), "RLum.Results")

  ## check length of output
  expect_equal(length(temp), 10)

  ## check for numerical regression
  results <- get_RLum(temp)
  expect_equal(results$de[1], 31.5299)
  expect_equal(results$de[2], 72.0333)
  expect_equal(results$de_err[1], 3.6387)
  expect_equal(results$de_err[2], 2.4082)
  expect_equal(results$proportion[1], 0.1096)
  expect_equal(results$proportion[2], 0.8904)

  ## test plot
  expect_s4_class(calc_FiniteMixture(
    ExampleData.DeValues$CA1,
    sigmab = 0.2,
    n.components = 2:3,
    grain.probability = TRUE,
    verbose = FALSE), "RLum.Results")

})

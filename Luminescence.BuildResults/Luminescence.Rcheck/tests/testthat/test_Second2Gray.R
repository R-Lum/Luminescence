context("Second2Gray")

data(ExampleData.DeValues, envir = environment())
results <- Second2Gray(ExampleData.DeValues$BT998, c(0.2,0.01))
results_alt1 <- Second2Gray(ExampleData.DeValues$BT998, c(0.2,0.01), error.propagation = "gaussian")
results_alt2 <- Second2Gray(ExampleData.DeValues$BT998, c(0.2,0.01), error.propagation = "absolute")

test_that("check class and length of output", {
  testthat::skip_on_cran()
  expect_is(results, class = "data.frame", info = NULL, label = NULL)

})

test_that("check values from output example", {
  testthat::skip_on_cran()
  expect_equal(sum(results[[1]]), 14754.09)
  expect_equal(sum(results[[2]]), 507.692)
  expect_equal(sum(results_alt1[[2]]), 895.911)
  expect_equal(sum(results_alt2[[2]]), 1245.398)

})

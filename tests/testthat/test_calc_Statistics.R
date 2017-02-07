context("calc_Statistics")

## load example data
data(ExampleData.DeValues, envir = environment())

## calculate statistics and show output
set.seed(1)
temp <- calc_Statistics(ExampleData.DeValues$BT998, n.MCM = 1000)
temp_alt1 <- calc_Statistics(ExampleData.DeValues$BT998, n.MCM = 1000, digits = 2)
temp_alt2 <- calc_Statistics(ExampleData.DeValues$BT998, n.MCM = 1000, digits = NULL)
temp_RLum <- set_RLum(class = "RLum.Results", data = list(data = ExampleData.DeValues$BT998))

test_that("check class and length of output", {
  testthat::skip_on_cran()
  expect_equal(is(temp), c("list", "vector"))
  expect_equal(length(temp), 3)

})

test_that("Test certain input scenarios", {
  expect_is(calc_Statistics(temp_RLum), "list")

  df <- ExampleData.DeValues$BT998
  df[,2] <- 0
  expect_warning(calc_Statistics(df))


})


test_that("check error messages", {
  testthat::skip_on_cran()
  expect_error(calc_Statistics(data = matrix(0,2)),
               regexp = "[calc_Statistics()] Input data is neither of type 'data.frame' nor 'RLum.Results'",
               fixed = TRUE)
  expect_error(calc_Statistics(data = df, weight.calc = "test"))

})


test_that("check weighted values from output", {
  testthat::skip_on_cran()
  expect_equal(temp$weighted$n, 25)
  expect_equal(sum(unlist(temp_alt1)),24535.72)
  expect_equal(sum(unlist(temp_alt2)),24534.1)
  expect_equal(round(temp$weighted$mean, digits = 3), 2896.036)
  expect_equal(round(temp$weighted$median, digits = 2), 2884.46)
  expect_equal(round(temp$weighted$sd.abs, digits = 4), 240.2228)
  expect_equal(round(temp$weighted$sd.rel, digits = 6), 8.294885)
  expect_equal(round(temp$weighted$se.abs, digits = 5), 48.04457)
  expect_equal(round(temp$weighted$se.rel, digits = 6), 1.658977)
  expect_equal(round(temp$weighted$skewness, digits = 6), 1.342018)
  expect_equal(round(temp$weighted$kurtosis, digits = 6), 4.387913)


})

test_that("check unweighted values from output", {
  testthat::skip_on_cran()

  expect_equal(temp$weighted$n, 25)
  expect_equal(round(temp$unweighted$mean, digits = 3), 2950.818)
  expect_equal(round(temp$unweighted$median, digits = 2), 2884.46)
  expect_equal(round(temp$unweighted$sd.abs, digits = 4), 281.6433)
  expect_equal(round(temp$unweighted$sd.rel, digits = 6), 9.544584)
  expect_equal(round(temp$unweighted$se.abs, digits = 5), 56.32866)
  expect_equal(round(temp$unweighted$se.rel, digits = 6), 1.908917)
  expect_equal(round(temp$unweighted$skewness, digits = 6), 1.342018)
  expect_equal(round(temp$unweighted$kurtosis, digits = 6), 4.387913)


})

test_that("check MCM values from output", {

  expect_equal(temp$MCM$n, 25)
  expect_equal(round(temp$MCM$mean, digits = 3), 2950.992)
  expect_equal(round(temp$MCM$median, digits = 3), 2885.622)
  expect_equal(round(temp$MCM$sd.abs, digits = 4), 295.0737)
  expect_equal(round(temp$MCM$sd.rel, digits = 6), 9.999137)
  expect_equal(round(temp$MCM$se.abs, digits = 5), 59.01474)
  expect_equal(round(temp$MCM$se.rel, digits = 6), 1.999827)
  expect_equal(round(temp$MCM$skewness, digits = 3), 1286.082)
  expect_equal(round(temp$MCM$kurtosis, digits = 3), 4757.097)


})

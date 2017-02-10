context("calc_OSLLxTxRatio")

data(ExampleData.LxTxOSLData, envir = environment())

temp <- calc_OSLLxTxRatio(
  Lx.data = Lx.data,
  Tx.data = Tx.data,
  signal.integral = c(1:2),
  background.integral = c(85:100))

test_that("check class and length of output", {
  testthat::skip_on_cran()
  expect_equal(is(temp), c("RLum.Results", "RLum"))
  expect_equal(length(temp), 2)

})

test_that("check values from output example", {
  testthat::skip_on_cran()

  results <- get_RLum(temp)

  expect_equal(results$LnLx, 81709)
  expect_equal(results$LnLx.BG, 530)
  expect_equal(results$TnTx, 7403)
  expect_equal(results$TnTx.BG, 513)
  expect_equal(results$Net_LnLx, 81179)
  expect_equal(round(results$Net_LnLx.Error, digits = 4), 286.5461)
  expect_equal(results$Net_TnTx, 6890)
  expect_equal(round(results$Net_TnTx.Error, digits = 5), 88.53581)
  expect_equal(round(results$LxTx, digits = 5), 11.78215)
  expect_equal(round(results$LxTx.Error, digits = 7), 0.1570077)

})

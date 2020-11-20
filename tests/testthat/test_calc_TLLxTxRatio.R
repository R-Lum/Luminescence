##load package example data
data(ExampleData.BINfileData, envir = environment())

##convert Risoe.BINfileData into a curve object
temp <- Risoe.BINfileData2RLum.Analysis(TL.SAR.Data, pos = 3)


Lx.data.signal <- get_RLum(temp, record.id=1)
Lx.data.background <- get_RLum(temp, record.id=2)
Tx.data.signal <- get_RLum(temp, record.id=3)
Tx.data.background <- get_RLum(temp, record.id=4)
signal.integral.min <- 210
signal.integral.max <- 230

temp <- calc_TLLxTxRatio(Lx.data.signal,
                           Lx.data.background,
                           Tx.data.signal, Tx.data.background,
                           signal.integral.min, signal.integral.max)

test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_s4_class(temp, "RLum.Results")
  expect_equal(length(temp), 1)

})

test_that("check values from output", {
  testthat::skip_on_cran()
  local_edition(3)

  results <- get_RLum(temp)

  expect_equal(length(results), 10)

  expect_equal(results$LnLx, 257042)
  expect_equal(results$LnLx.BG, 4068)
  expect_equal(results$TnTx, 82298)
  expect_equal(results$TnTx.BG, 2943)
  expect_equal(results$net_LnLx, 252974)
  expect_equal(round(results$net_LnLx.Error, digits = 2), 49468.92)
  expect_equal(results$net_TnTx, 79355)
  expect_equal(round(results$net_TnTx.Error,2), 21449.72)
  expect_equal(round(results$LxTx, digits =  6), 3.187877)
  expect_equal(round(results$LxTx.Error, digits = 6), 1.485073)

})

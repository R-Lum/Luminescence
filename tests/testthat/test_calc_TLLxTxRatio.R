## load data
data(ExampleData.BINfileData, envir = environment())

## convert Risoe.BINfileData into a curve object
temp <- Risoe.BINfileData2RLum.Analysis(TL.SAR.Data, pos = 3)
Lx.data.signal <- get_RLum(temp, record.id = 1)
Lx.data.background <- get_RLum(temp, record.id = 2)
Tx.data.signal <- get_RLum(temp, record.id = 3)
Tx.data.background <- get_RLum(temp, record.id = 4)
signal_integral <- 210:230

test_that("input validation", {
  testthat::skip_on_cran()

    ## different data types
    expect_error(calc_TLLxTxRatio(
      Lx.data.signal,
      Lx.data.background,
      Tx.data.signal = as.data.frame(Tx.data.signal),
      Tx.data.background,
      signal_integral),
      "[calc_TLLxTxRatio()] Data types of Lx and Tx data differ",
      fixed = TRUE)

    ## different data types
    expect_error(calc_TLLxTxRatio(
      Lx.data.signal,
      Lx.data.background,
      Tx.data.signal = as.data.frame(Tx.data.signal),
      Tx.data.background,
      signal_integral),
      "[calc_TLLxTxRatio()] Data types of Lx and Tx data differ",
      fixed = TRUE)

    ## check for allowed data types
    expect_error(calc_TLLxTxRatio(
      Lx.data.signal = as.matrix(Tx.data.signal),
      Lx.data.background,
      Tx.data.signal = as.matrix(Tx.data.signal),
      Tx.data.background,
      signal_integral),
      "'Lx.data.signal' should be of class 'data.frame' or 'RLum.Data.Curve'")
    expect_error(calc_TLLxTxRatio(
      Lx.data.signal = Lx.data.signal,
      Lx.data.background,
      Tx.data.signal = Tx.data.signal,
      Tx.data.background,
      signal_integral = "error"),
      "'signal_integral' should be of class 'integer' or 'numeric'")

    ## check for different channel numbers
    expect_error(calc_TLLxTxRatio(
      Lx.data.signal = as.data.frame(Tx.data.signal)[1:10,],
      Lx.data.background,
      Tx.data.signal = as.data.frame(Tx.data.signal),
      Tx.data.background,
      signal_integral),
      "[calc_TLLxTxRatio()] Channel numbers differ for Lx and Tx data",
      fixed = TRUE)
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## trigger warning
  expect_warning(calc_TLLxTxRatio(
      Lx.data.signal,
      Lx.data.background,
      Tx.data.signal,
      Tx.data.background = Lx.data.background,
      signal_integral),
      regexp = "\\[calc\\_TLLxTxRatio\\(\\)\\] The background signals for Lx and Tx appear to be similar.+")

  ## use invalid signal integral
  expect_warning(calc_TLLxTxRatio(
      Lx.data.signal,
      Lx.data.background,
      Tx.data.signal,
      Tx.data.background,
      signal_integral = 10:1000),
      "'signal_integral' out of bounds, reset to be between 10 and 250")

  ## integral_input
  expect_equal(calc_TLLxTxRatio(Lx.data.signal,
                                Lx.data.background,
                                Tx.data.signal,
                                Tx.data.background,
                                signal_integral = 378:414,
                                integral_input = "measurement")$LxTx.table,
               calc_TLLxTxRatio(Lx.data.signal,
                                Lx.data.background,
                                Tx.data.signal,
                                Tx.data.background,
                                signal_integral = 210:230,
                                integral_input = "channel")$LxTx.table)

  ## deprecated arguments
  expect_warning(calc_TLLxTxRatio(
      Lx.data.signal,
      Lx.data.background,
      Tx.data.signal,
      Tx.data.background,
      signal.integral = signal_integral),
      "was deprecated in v1.2.0, use 'signal_integral'")
  expect_warning(expect_error(calc_TLLxTxRatio(
      Lx.data.signal,
      Lx.data.background,
      Tx.data.signal,
      Tx.data.background,
      signal.integral = signal_integral,
      integral_input = "measurement"),
      "'integral_input' is not supported with old argument names"),
      "was deprecated in v1.2.0, use 'signal_integral'")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  ## run function without error
  expect_snapshot_RLum(calc_TLLxTxRatio(
    Lx.data.signal,
    Lx.data.background,
    Tx.data.signal,
    Tx.data.background,
    signal_integral))

  expect_snapshot_RLum(calc_TLLxTxRatio(
    Lx.data.signal,
    Lx.data.background=NULL,
    Tx.data.signal,
    Tx.data.background=NULL,
    signal_integral))
})

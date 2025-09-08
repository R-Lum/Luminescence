## load data
data(ExampleData.BINfileData, envir = environment())

## convert Risoe.BINfileData into a curve object
temp <- Risoe.BINfileData2RLum.Analysis(TL.SAR.Data, pos = 3)
Lx.data.signal <- get_RLum(temp, record.id = 1)
Lx.data.background <- get_RLum(temp, record.id = 2)
Tx.data.signal <- get_RLum(temp, record.id = 3)
Tx.data.background <- get_RLum(temp, record.id = 4)
signal.integral.min <- 210
signal.integral.max <- 230

test_that("input validation", {
  testthat::skip_on_cran()

    ## different data types
    expect_error(calc_TLLxTxRatio(
      Lx.data.signal,
      Lx.data.background,
      Tx.data.signal = as.data.frame(Tx.data.signal),
      Tx.data.background,
      signal.integral.min,
      signal.integral.max),
      "[calc_TLLxTxRatio()] Data types of Lx and Tx data differ",
      fixed = TRUE)

    ## different data types
    expect_error(calc_TLLxTxRatio(
      Lx.data.signal,
      Lx.data.background,
      Tx.data.signal = as.data.frame(Tx.data.signal),
      Tx.data.background,
      signal.integral.min,
      signal.integral.max),
      "[calc_TLLxTxRatio()] Data types of Lx and Tx data differ",
      fixed = TRUE)

    ## check for allowed data types
    expect_error(calc_TLLxTxRatio(
      Lx.data.signal = as.matrix(Tx.data.signal),
      Lx.data.background,
      Tx.data.signal = as.matrix(Tx.data.signal),
      Tx.data.background,
      signal.integral.min,
      signal.integral.max),
      "'Lx.data.signal' should be of class 'data.frame' or 'RLum.Data.Curve'")
    expect_error(calc_TLLxTxRatio(
      Lx.data.signal = Lx.data.signal,
      Lx.data.background,
      Tx.data.signal = Tx.data.signal,
      Tx.data.background,
      signal.integral.min = "error",
      signal.integral.max),
      "'signal.integral.min' should be a positive integer scalar")
    expect_error(calc_TLLxTxRatio(
      Lx.data.signal = Lx.data.signal,
      Lx.data.background,
      Tx.data.signal = Tx.data.signal,
      Tx.data.background,
      signal.integral.min,
      signal.integral.max = "error"),
      "'signal.integral.max' should be a positive integer scalar")

    ## check for different channel numbers
    expect_error(calc_TLLxTxRatio(
      Lx.data.signal = as.data.frame(Tx.data.signal)[1:10,],
      Lx.data.background,
      Tx.data.signal = as.data.frame(Tx.data.signal),
      Tx.data.background,
      signal.integral.min,
      signal.integral.max),
      "[calc_TLLxTxRatio()] Channel numbers differ for Lx and Tx data",
      fixed = TRUE)

    ## use invalid signal integral
    expect_error(calc_TLLxTxRatio(
      Lx.data.signal,
      Lx.data.background,
      Tx.data.signal,
      Tx.data.background,
      signal.integral.min = 10,
      signal.integral.max = 1000),
      "[calc_TLLxTxRatio()] 'signal.integral' is not valid",
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
      signal.integral.min,
      signal.integral.max),
      regexp = "\\[calc\\_TLLxTxRatio\\(\\)\\] The background signals for Lx and Tx appear to be similar.+")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  ## run function without error
  expect_snapshot_RLum(calc_TLLxTxRatio(
    Lx.data.signal,
    Lx.data.background,
    Tx.data.signal,
    Tx.data.background,
    signal.integral.min,
    signal.integral.max))

  expect_snapshot_RLum(calc_TLLxTxRatio(
    Lx.data.signal,
    Lx.data.background=NULL,
    Tx.data.signal,
    Tx.data.background=NULL,
    signal.integral.min,
    signal.integral.max))
})

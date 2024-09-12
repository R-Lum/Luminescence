test_that("calc_TLLxTxRatio", {
  testthat::skip_on_cran()

  ##load package example data
  data(ExampleData.BINfileData, envir = environment())

  ##convert Risoe.BINfileData into a curve object
  temp <- Risoe.BINfileData2RLum.Analysis(TL.SAR.Data, pos = 3)
  Lx.data.signal <- get_RLum(temp, record.id = 1)
  Lx.data.background <- get_RLum(temp, record.id = 2)
  Tx.data.signal <- get_RLum(temp, record.id = 3)
  Tx.data.background <- get_RLum(temp, record.id = 4)
  signal.integral.min <- 210
  signal.integral.max <- 230

  ## break the function
    ## different data types
    expect_error(calc_TLLxTxRatio(
      Lx.data.signal,
      Lx.data.background,
      Tx.data.signal = as.data.frame(Tx.data.signal),
      Tx.data.background,
      signal.integral.min,
      signal.integral.max),
    regexp = "\\[calc\\_TLLxTxRatio\\(\\)\\] Data types of Lx and Tx data differ.+")

    ## different data types
    expect_error(calc_TLLxTxRatio(
      Lx.data.signal,
      Lx.data.background,
      Tx.data.signal = as.data.frame(Tx.data.signal),
      Tx.data.background,
      signal.integral.min,
      signal.integral.max),
      regexp = "\\[calc\\_TLLxTxRatio\\(\\)\\] Data types of Lx and Tx data differ.+")

    ## check for allowed data types
    expect_error(calc_TLLxTxRatio(
      Lx.data.signal = as.matrix(Tx.data.signal),
      Lx.data.background,
      Tx.data.signal = as.matrix(Tx.data.signal),
      Tx.data.background,
      signal.integral.min,
      signal.integral.max),
      regexp = "\\[calc\\_TLLxTxRatio\\(\\)\\] Input data type for not allowed.+")

    ## check for different channel numbers
    expect_error(calc_TLLxTxRatio(
      Lx.data.signal = as.data.frame(Tx.data.signal)[1:10,],
      Lx.data.background,
      Tx.data.signal = as.data.frame(Tx.data.signal),
      Tx.data.background,
      signal.integral.min,
      signal.integral.max),
      regexp = "\\[calc\\_TLLxTxRatio\\(\\)\\] Channel numbers differ for Lx and Tx data.+")

    ## use invalid signal integral
    expect_error(calc_TLLxTxRatio(
      Lx.data.signal,
      Lx.data.background,
      Tx.data.signal,
      Tx.data.background,
      signal.integral.min = 10,
      signal.integral.max = 1000),
      regexp = "\\[calc\\_TLLxTxRatio\\(\\)\\] signal.integral is not valid.+")

  ## trigger warning
  expect_warning(calc_TLLxTxRatio(
      Lx.data.signal,
      Lx.data.background,
      Tx.data.signal,
      Tx.data.background = Lx.data.background,
      signal.integral.min,
      signal.integral.max),
      regexp = "\\[calc\\_TLLxTxRatio\\(\\)\\] The background signals for Lx and Tx appear to be similar.+")

  ## run function without error
  temp <- expect_s4_class(calc_TLLxTxRatio(
    Lx.data.signal,
    Lx.data.background,
    Tx.data.signal,
    Tx.data.background,
    signal.integral.min,
    signal.integral.max), class = "RLum.Results")

  ## check lenght
  expect_equal(length(temp), 1)

  ## extract elements
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

  expect_s4_class(calc_TLLxTxRatio(
    Lx.data.signal,
    Lx.data.background=NULL,
    Tx.data.signal,
    Tx.data.background=NULL,
    signal.integral.min,
    signal.integral.max), class = "RLum.Results")
})

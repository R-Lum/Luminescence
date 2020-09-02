context("analyse_SAR.CWOSL")

  ##prepare test file
  set.seed(1)
  data(ExampleData.BINfileData, envir = environment())
  object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1:2 )

  results <- analyse_SAR.CWOSL(
    object = object[[1]],
    signal.integral.min = 1,
    signal.integral.max = 2,
    background.integral.min = 900,
    background.integral.max = 1000,
    log = "x",
    fit.method = "EXP",
    rejection.criteria = list(
      recycling.ratio = 10,
      recuperation.rate = 10,
      testdose.error = 10,
      palaeodose.error = 10,
      exceed.max.regpoint = TRUE),
    plot = FALSE,
    verbose = FALSE
  )

test_that("simple run", {
    testthat::skip_on_cran()

  ##perform SAR tests with different parameters
  ##verbose and plot off
  expect_s4_class(
    analyse_SAR.CWOSL(
      object = object,
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 1000,
      fit.method = "EXP",
      plot = FALSE,
      verbose = FALSE
    ),
    class = "RLum.Results"
  )

  ##verbose and plot on
  expect_s4_class(
    analyse_SAR.CWOSL(
      object = object[[1]],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 1000,
      fit.method = "LIN"
    ),
    class = "RLum.Results"
  )

  ##plot single
  expect_s4_class(
    analyse_SAR.CWOSL(
      object = object[[1]],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 1000,
      fit.method = "LIN",
      plot = TRUE,
      plot.single = TRUE
    ),
    class = "RLum.Results"
  )

  ##trigger warnings
  expect_warning(analyse_SAR.CWOSL(
      object = object[[1]],
      background.integral.min = 900,
      background.integral.max = 1000,
      fit.method = "LIN",
      plot = FALSE,
      verbose = FALSE
  ), regexp = "'signal.integral.min' missing, set to 1")

 ##trigger stops for parameters
 ##object
  expect_error(analyse_SAR.CWOSL(
    object = "fail",
    background.integral.min = 900,
    fit.method = "LIN",
    plot = FALSE,
    verbose = FALSE
  ), regexp = "Input object is not of type 'RLum.Analysis'!")

 expect_error(suppressWarnings(analyse_SAR.CWOSL(
    object = object[[1]],
    background.integral.min = 900,
    fit.method = "LIN",
    plot = FALSE,
    verbose = FALSE
  )), regexp = "No value set for 'background.integral.max'!")

 expect_error(suppressWarnings(analyse_SAR.CWOSL(
   object = object[[1]],
   background.integral.max = 900,
   fit.method = "LIN",
   plot = FALSE,
   verbose = FALSE
 )), regexp = "No value set for 'background.integral.min'!")


})

test_that("tests class elements", {
  testthat::skip_on_cran()

    expect_is(results, "RLum.Results")
    expect_equal(length(results), 4)
    expect_is(results$data, "data.frame")
    expect_is(results$LnLxTnTx.table, "data.frame")
    expect_is(results$rejection.criteria, "data.frame")
    expect_is(results$Formula, "expression")

})

test_that("regression tests De values", {
  testthat::skip_on_cran()

  ##fix for different R versions
  if(R.version$major == "3" && as.numeric(R.version$minor) < 6){
   expect_equal(object = round(sum(results$data[1:2]), digits = 0), 1717)

  }else{
    expect_equal(object = round(sum(results$data[1:2]), digits = 0), 1716)

  }

})

test_that("regression test LxTx table", {
  testthat::skip_on_cran()

   expect_equal(object = round(sum(results$LnLxTnTx.table$LxTx), digits = 5),  20.92051)
   expect_equal(object = round(sum(results$LnLxTnTx.table$LxTx.Error), digits = 2), 0.34)

})

test_that("regression test - check rejection criteria", {
  testthat::skip_on_cran()
  ##fix for different R versions
  if(R.version$major == "3" && as.numeric(R.version$minor) < 6){
    expect_equal(object = round(sum(results$rejection.criteria$Value), digits = 0),  1669)

  }else{
   expect_equal(object = round(sum(results$rejection.criteria$Value), digits = 0),  1669)

  }

})


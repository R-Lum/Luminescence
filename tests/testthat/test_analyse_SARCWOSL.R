context("analyse_SAR.CWOSL")

set.seed(1)
data(ExampleData.BINfileData, envir = environment())
object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

##perform SAR analysis and set rejection criteria
results <- analyse_SAR.CWOSL(
object = object,
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

test_that("check class and length of output", {
  testthat::skip_on_cran()

    expect_is(results, "RLum.Results")
    expect_equal(length(results), 4)
    expect_is(results$data, "data.frame")
    expect_is(results$LnLxTnTx.table, "data.frame")
    expect_is(results$rejection.criteria, "data.frame")
    expect_is(results$Formula, "expression")

})

test_that("check De values", {
  testthat::skip_on_cran()

  ##fix for different R versions
  if(R.version$major == "3" && as.numeric(R.version$minor) < 6){
   expect_equal(object = round(sum(results$data[1:2]), digits = 0), 1717)

  }else{
    expect_equal(object = round(sum(results$data[1:2]), digits = 0), 1716)

  }

})

test_that("check LxTx table", {
  testthat::skip_on_cran()

   expect_equal(object = round(sum(results$LnLxTnTx.table$LxTx), digits = 5),  20.92051)
   expect_equal(object = round(sum(results$LnLxTnTx.table$LxTx.Error), digits = 2), 0.34)

})

test_that("check rejection criteria", {
  testthat::skip_on_cran()
  ##fix for different R versions
  if(R.version$major == "3" && as.numeric(R.version$minor) < 6){
    expect_equal(object = round(sum(results$rejection.criteria$Value), digits = 0),  1669)

  }else{
   expect_equal(object = round(sum(results$rejection.criteria$Value), digits = 0),  1669)

  }

})


test_that("Check subset_SingleGrain", {
  testthat::skip_on_cran()

  ## get example ready
  data(ExampleData.BINfileData, envir = environment())

  ## set POSITION/GRAIN pair dataset
  selection <- data.frame(POSITION = c(1,5,7), GRAIN = c(0,0,0))

  ## crash function
  expect_error(subset_SingleGrainData("error"),
               "[subset_SingleGrainData()] 'object' should be of class 'Risoe.BINfileData'",
               fixed = TRUE)

  ## standard run
  expect_s4_class(subset_SingleGrainData(object = CWOSL.SAR.Data, selection = selection), "Risoe.BINfileData")

  ## try something different for the input
  expect_s4_class(subset_SingleGrainData(object = CWOSL.SAR.Data, selection = as.matrix(selection)), "Risoe.BINfileData")

})

test_that("Check subset_SingleGrain", {
  testthat::skip_on_cran()
  local_edition(3)

  ## get example ready
  data(ExampleData.BINfileData, envir = environment())

  ## set POSITION/GRAIN pair dataset
  selection <- data.frame(POSITION = c(1,5,7), GRAIN = c(0,0,0))

  ## crash function
  expect_error(
    object = subset_SingleGrainData("error"),
    regexp = "\\[subset\\_SingleGrainData\\(\\)\\] Only Risoe.BINfileData-class objects are allowed as input!")

  ## standard run
  expect_s4_class(subset_SingleGrainData(object = CWOSL.SAR.Data, selection = selection), "Risoe.BINfileData")

  ## try something different for the input
  expect_s4_class(subset_SingleGrainData(object = CWOSL.SAR.Data, selection = as.matrix(selection)), "Risoe.BINfileData")

})

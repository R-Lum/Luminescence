## load data
data(ExampleData.BINfileData, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(subset_SingleGrainData("error"),
               "'object' should be of class 'Risoe.BINfileData'")
  expect_error(subset_SingleGrainData(CWOSL.SAR.Data, NA),
               "'selection' should be of class 'data.frame' or 'matrix'")
  expect_error(subset_SingleGrainData(CWOSL.SAR.Data, data.frame(1:10, 1)),
               "No matching records, check POSITION and GRAIN in 'selection'")
  expect_error(subset_SingleGrainData(CWOSL.SAR.Data, data.frame(1:10)),
               "'selection' should have 2 columns")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## set POSITION/GRAIN pair dataset
  selection <- data.frame(POSITION = c(1,5,7), GRAIN = c(0,0,0))

  ## standard run
  expect_s4_class(subset_SingleGrainData(object = CWOSL.SAR.Data, selection = selection), "Risoe.BINfileData")

  ## try something different for the input
  expect_s4_class(subset_SingleGrainData(object = CWOSL.SAR.Data, selection = as.matrix(selection)), "Risoe.BINfileData")
})

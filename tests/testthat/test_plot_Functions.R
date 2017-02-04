context("Test Various Plot Functions")


test_that("test pure success of the plotting without warning or error", {

  ##distribution plots
  data(ExampleData.DeValues, envir = environment())
  ExampleData.DeValues <- ExampleData.DeValues$CA1

  expect_silent(plot_AbanicoPlot(ExampleData.DeValues))
  expect_silent(plot_RadialPlot(ExampleData.DeValues))
  expect_silent(plot_KDE(ExampleData.DeValues))
  expect_silent(plot_Histogram(ExampleData.DeValues))
  expect_silent(plot_ViolinPlot(ExampleData.DeValues))


  ##plot NRT
  data("ExampleData.BINfileData", envir = environment())
  data <- Risoe.BINfileData2RLum.Analysis(object = CWOSL.SAR.Data, pos = 8, ltype = "OSL")
  allCurves <- get_RLum(data)
  pos <- seq(1, 9, 2)
  curves <- allCurves[pos]
  expect_silent(plot_NRt(curves))

  ##filter combinations
  filter1 <- density(rnorm(100, mean = 450, sd = 20))
  filter1 <- matrix(c(filter1$x, filter1$y/max(filter1$y)), ncol = 2)
  filter2 <- matrix(c(200:799,rep(c(0,0.8,0),each = 200)), ncol = 2)

  ## Example 1 (standard)
  expect_silent(plot_FilterCombinations(filters = list(filter1, filter2)))

   ##plot_Det
  data(ExampleData.BINfileData, envir = environment())
  object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

  expect_is(plot_DetPlot(object,
               signal.integral.min = 1,
               signal.integral.max = 3,
               background.integral.min = 900,
               background.integral.max = 1000,
               n.channels = 5,
  ), "RLum.Results")



})


test_that("test for return values, if any", {
  data(ExampleData.DeValues, envir = environment())
  output <- plot_AbanicoPlot(ExampleData.DeValues, output = TRUE)
    expect_is(output, "list")
    expect_length(output, 10)
})

context("Test De-Distribution Plots")

data(ExampleData.DeValues, envir = environment())
ExampleData.DeValues <- ExampleData.DeValues$CA1

test_that("test pure success of the plotting without warning or error", {
  expect_silent(plot_AbanicoPlot(ExampleData.DeValues))
  expect_silent(plot_RadialPlot(ExampleData.DeValues))
  expect_silent(plot_KDE(ExampleData.DeValues))
  expect_silent(plot_Histogram(ExampleData.DeValues))
  expect_silent(plot_ViolinPlot(ExampleData.DeValues))

})


test_that("test for return values, if any", {
  output <- plot_AbanicoPlot(ExampleData.DeValues, output = TRUE)
    expect_is(output, "list")
    expect_length(output, 10)
})

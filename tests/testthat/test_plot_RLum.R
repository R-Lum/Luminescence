test_that("test_plot_RLum", {
  testthat::skip_on_cran()
  local_edition(3)

  ## create dataset to test
  image <- as(array(rnorm(1000), dim = c(10,10,10)), "RLum.Data.Image")
  expect_silent(plot_RLum(image))

  ## check list with different dispatched arguments
  image_short <- as(array(rnorm(100), dim = c(10, 10, 1)), "RLum.Data.Image")
  expect_silent(plot_RLum(list(image_short, image_short), main = list("test1", "test2"), mtext = "test"))

  ## trigger error
  expect_error(plot_RLum("error"), "\\[plot_RLum\\(\\)\\] Sorry, I don't know what to do for object of type 'character'.")

  ## test list of RLum.Analysis
  l <- list(set_RLum(
    class = "RLum.Analysis",
    records = list(
      set_RLum("RLum.Data.Curve", data = matrix(1:10, ncol = 2)),
      set_RLum("RLum.Data.Curve", data = matrix(1:20, ncol = 2)))))

  expect_silent(plot_RLum(l, main = list("test", "test2"), mtext = "test"))

  ## plot results objects
  data(ExampleData.BINfileData, envir = environment())
  object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1:3)
  results <- analyse_SAR.CWOSL(
    object = object,
    signal.integral.min = 1,
    signal.integral.max = 2,
    plot = FALSE,
    verbose = FALSE,
    background.integral.min = 900,
    background.integral.max = 1000,
    fit.method = "LIN")
  expect_null(plot_RLum.Results(results))


})

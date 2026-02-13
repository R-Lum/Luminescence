test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_RLum("error"),
               "[plot_RLum()] 'object' should be of class 'RLum'",
               fixed = TRUE)
})

test_that("empty objects", {
  testthat::skip_on_cran()

  expect_message(plot_RLum(set_RLum("RLum.Analysis")),
                 "Nothing to plot, NULL returned")
  expect_silent(plot_RLum(set_RLum("RLum.Data.Curve")))
  expect_silent(plot_RLum(set_RLum("RLum.Data.Image")))
  expect_error(plot_RLum(set_RLum("RLum.Data.Spectrum")),
               "'object' contains no data")
  expect_silent(plot_RLum(set_RLum("RLum.Results")))
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## create dataset to test
  image <- as(array(rnorm(1000), dim = c(10,10,10)), "RLum.Data.Image")
  expect_silent(plot_RLum(image))

  ## check list with different dispatched arguments
  image_short <- as(array(rnorm(100), dim = c(10, 10, 1)), "RLum.Data.Image")
  expect_silent(plot_RLum(list(image_short, image_short), main = list("test1", "test2"), mtext = "test"))

  ## plot results objects
  data(ExampleData.BINfileData, envir = environment())
  object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1:3)
  results <- analyse_SAR.CWOSL(
    object = object,
    signal_integral = 1:2,
    background_integral = 900:1000,
    plot = FALSE,
    verbose = FALSE,
    fit.method = "LIN")
  expect_null(plot_RLum(results))
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  ## test list of RLum.Analysis
  obj <- set_RLum(
    class = "RLum.Analysis",
    records = list(
      set_RLum("RLum.Data.Curve", data = matrix(1:10, ncol = 2)),
      set_RLum("RLum.Data.Curve", data = matrix(1:20, ncol = 2))))

  SW({
  vdiffr::expect_doppelganger("RLum.Analysis main",
                              plot_RLum(obj, main = list("a", "b")))
  vdiffr::expect_doppelganger("RLum.Analysis list main",
                              plot_RLum(list(obj, obj), main = list("a", "b")))
  vdiffr::expect_doppelganger("RLum.Analysis list mtext sub",
                              plot_RLum(list(obj, obj), mtext = "test",
                                        sub = "sub"))
  })
})

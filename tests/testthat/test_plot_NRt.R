data("ExampleData.BINfileData", envir = environment())
obj <- Risoe.BINfileData2RLum.Analysis(object = CWOSL.SAR.Data,
                                       pos = 8, ltype = "OSL")
curves <- get_RLum(obj)[seq(1, 9, 2)]

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_NRt(curves[1]),
                "The provided list only contains curve data of the natural signal")
  expect_error(plot_NRt(curves[[1]]@data),
               "The provided matrix only contains curve data of the natural signal")
})

test_that("check", {
  testthat::skip_on_cran()

  ## list
  expect_silent(plot_NRt(curves))
  expect_silent(plot_NRt(curves, smooth = "spline", log = "x"))
  expect_silent(plot_NRt(curves, smooth = "rmean", k = 10))

  ## matrix
  plot_NRt(cbind(curves[[1]]@data, curves[[1]]@data))

  ## RLum.Analysis
  expect_silent(plot_NRt(obj))
})

## load data
data("ExampleData.BINfileData", envir = environment())
obj <- Risoe.BINfileData2RLum.Analysis(object = CWOSL.SAR.Data,
                                       pos = 8, ltype = "OSL")
curves <- get_RLum(obj)[seq(1, 9, 2)]

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_NRt("error"),
               "'data' should be of class 'list', 'data.frame', 'matrix' or")
  expect_error(plot_NRt(obj[[2]]),
               "'data' should be of class 'list', 'data.frame', 'matrix' or")
  expect_error(plot_NRt(curves[1]),
                "'data' contains only curve data for the natural signal")
  expect_error(plot_NRt(curves[[1]]@data),
                "'data' contains only curve data for the natural signal")
  expect_error(plot_NRt(curves, smooth = "error"),
               "'smooth' should be one of 'none', 'spline' or 'rmean'")

  data(ExampleData.XSYG, envir = environment())
  obj.mixed <- merge_RLum.Analysis(list(obj, TL.Spectrum))
  expect_error(plot_NRt(obj.mixed),
               "The provided 'RLum.Analysis' object must exclusively contain")
  expect_error(plot_NRt(set_RLum("RLum.Analysis",
                                 records = list(curves[[1]]))),
               "'data' contains only curve data for the natural signal")

  data("ExampleData.RLum.Analysis", envir = environment())
  expect_error(plot_NRt(IRSAR.RF.Data),
               "The time values for the natural signal don't match those for")
  expect_error(plot_NRt(merge_RLum.Analysis(list(obj, IRSAR.RF.Data))),
               "The time values for the natural signal don't match those for")

  data(ExampleData.CW_OSL_Curve, envir = environment())
  expect_error(plot_NRt(ExampleData.CW_OSL_Curve),
               "'data' contains only curve data for the natural signal")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## list
  expect_silent(plot_NRt(curves))
  expect_silent(plot_NRt(curves, smooth = "spline", log = "x"))

  small <- curves[1:3]
  for (idx in 1:length(small))
    small[[idx]]@data <- small[[idx]]@data[1:175, ]
  expect_snapshot_RLum(plot_NRt(small, smooth = "rmean", k = 10))

  ## matrix
  plot_NRt(cbind(curves[[1]]@data, curves[[1]]@data))

  ## RLum.Analysis
  expect_silent(plot_NRt(obj))

  ## list of RLum.Analysis
  bin.v8 <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
  expect_output(plot_NRt(read_BIN2R(bin.v8, fastForward = TRUE, verbose = FALSE)),
                "BIN/BINX-file non-conform. TL curve may be wrong")
})

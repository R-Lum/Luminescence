## create dataset
data(ExampleData.CW_OSL_Curve, envir = environment())
temp <- as(ExampleData.CW_OSL_Curve, "RLum.Data.Curve")

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_RLum.Data.Curve("error"),
               "'object' should be of class 'RLum.Data.Curve'")
  expect_error(plot_RLum.Data.Curve(temp, norm = "error"),
               "'norm' should be one of 'max', 'last', 'huot'")

  temp_NA <- temp
  temp_NA@data[] <- suppressWarnings(NA_real_)
  expect_warning(expect_null(plot_RLum.Data.Curve(temp_NA)),
                 "Curve contains only NA-values, nothing plotted")
  expect_warning(plot_RLum.Data.Curve(set_RLum("RLum.Data.Curve"), norm = TRUE),
                 "Curve normalisation produced Inf/NaN values, values replaced by 0")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## run function with various conditions
  expect_silent(plot_RLum.Data.Curve(temp))
  expect_silent(plot_RLum.Data.Curve(temp, norm = TRUE))
  expect_silent(plot_RLum.Data.Curve(temp, norm = "max"))
  expect_silent(plot_RLum.Data.Curve(temp, norm = "last"))
  expect_silent(plot_RLum.Data.Curve(temp, norm = "huot"))
  expect_silent(plot_RLum.Data.Curve(temp, main = "title", col = "red"))
  expect_silent(plot_RLum.Data.Curve(temp, auto_scale = TRUE, xlim = c(10,20)))
  expect_silent(plot_RLum.Data.Curve(temp, auto_scale = TRUE, ylim = c(1,200)))
  expect_silent(plot_RLum.Data.Curve(temp, smooth = TRUE))
  expect_silent(plot_RLum.Data.Curve(temp, par.local = FALSE))

  temp@recordType <- "OSL"
  temp@info <- list(interval = 1)
  expect_silent(plot_RLum.Data.Curve(temp))
  temp@recordType <- "TL"
  temp@info <- list(curveDescripter = "xlab;ylab", RATE = 2)
  expect_silent(plot_RLum.Data.Curve(temp))
})

test_that("check interactive mode", {
  testthat::skip_on_cran()

  ## simple plot
  expect_silent(plot_RLum.Data.Curve(temp, interactive = TRUE))

  ## with arguments
  expect_silent(plot_RLum.Data.Curve(
    object = temp,
    interactive = TRUE,
    lty = 2,
    col = "red",
    log = "xy"))

})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not(getRversion() >= "4.4.0")

  SW({
  vdiffr::expect_doppelganger("default",
                              plot_RLum.Data.Curve(temp))
  vdiffr::expect_doppelganger("autoscale-xlim-smooth",
                              plot_RLum.Data.Curve(temp, auto_scale = TRUE,
                                                   xlim = c(10, 20),
                                                   smooth = TRUE))
  vdiffr::expect_doppelganger("autoscale-ylim-mtext-cex",
                              plot_RLum.Data.Curve(temp, auto_scale = TRUE,
                                                   ylim = c(1, 200),
                                                   mtext = "Test", cex = 2))
  })
})

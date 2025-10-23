## create dataset to test
set.seed(1)
image <- as(array(rnorm(1000), dim = c(10,10,10)), "RLum.Data.Image")

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_RLum.Data.Image("image"),
               "[plot_RLum.Data.Image()] 'object' should be of class 'RLum.Data.Image'",
               fixed = TRUE)
  expect_error(plot_RLum.Data.Image(image, plot.type = "error"),
               "[plot_RLum.Data.Image()] 'plot.type' should be one of 'plot.raster'",
               fixed = TRUE)
})

test_that("test functionality", {
  testthat::skip_on_cran()

  expect_silent(plot_RLum.Data.Image(image, plot.type = "plot.raster",
                                     frames = c(2, 4)))

  ## empty image
  expect_null(plot_RLum.Data.Image(set_RLum("RLum.Data.Image")))
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  SW({
  vdiffr::expect_doppelganger("raster",
                              plot_RLum.Data.Image(image,
                                                   plot.type = "plot.raster"))
  vdiffr::expect_doppelganger("raster lin stretch",
                              plot_RLum.Data.Image(image,
                                                   plot.type = "plot.raster",
                                                   frames = 10,
                                                   mtext = "Test",
                                                   stretch = "lin"))
  vdiffr::expect_doppelganger("raster zlim_image",
                              plot_RLum.Data.Image(image,
                                                   plot.type = "plot.raster",
                                                   frames = 1,
                                                   stretch = NULL,
                                                   zlim_image = c(0, 1)))
  vdiffr::expect_doppelganger("contour",
                              plot_RLum.Data.Image(image,
                                                   plot.type = "contour",
                                                   mtext = "Test",
                                                   frames = 5))
  })
})

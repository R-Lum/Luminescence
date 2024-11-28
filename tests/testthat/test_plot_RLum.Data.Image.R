## create dataset to test
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

test_that("Test image plotting", {
  testthat::skip_on_cran()

    ## plot.raster ---
    expect_silent(plot_RLum.Data.Image(image, plot.type = "plot.raster"))
    expect_silent(plot_RLum.Data.Image(image, plot.type = "plot.raster",
                                       stretch = NULL))
    expect_silent(plot_RLum.Data.Image(image, plot.type = "plot.raster",
                                       stretch = "lin"))
    expect_silent(plot_RLum.Data.Image(image, plot.type = "plot.raster",
                                       frames = c(2, 4)))

    ## check global z-scale
    expect_silent(plot_RLum.Data.Image(image, plot.type = "plot.raster",
                                       stretch = NULL, zlim_image = c(0,1)))

    ## contour ---
    expect_silent(plot_RLum.Data.Image(image, plot.type = "contour",
                                       stretch = NULL))

  ## empty image
  expect_null(plot_RLum.Data.Image(set_RLum("RLum.Data.Image")))
})

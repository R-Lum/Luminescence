test_that("Test image plotting", {
  testthat::skip_on_cran()
  local_edition(3)

    ## create dataset to test
    image <- as(array(rnorm(1000), dim = c(10,10,10)), "RLum.Data.Image")

    ## crash function ----
    ### wrong input -----
    expect_error(plot_RLum.Data.Image("image"),
                 "\\[plot_RLum.Data.Image\\(\\)\\] Input object is not of type RLum.Data.Image.")
    expect_error(plot_RLum.Data.Image(image, plot.type = "error"),
                 "\\[plot_RLum.Data.Image\\(\\)\\] Unknown plot type.")


    ## plot.raster ---
    expect_silent(plot_RLum.Data.Image(image, plot.type = "plot.raster"))
    expect_silent(plot_RLum.Data.Image(image, plot.type = "plot.raster",
                                       stretch = NULL))

    ## contour ---
    expect_silent(plot_RLum.Data.Image(image, plot.type = "contour",
                                       stretch = NULL))

})


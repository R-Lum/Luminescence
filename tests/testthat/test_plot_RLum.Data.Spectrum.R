context("Test Various Plot Functions")


test_that("test pure success of the plotting without warning or error", {
  testthat::skip_on_cran()

    ##RLum.Data.Spectrum -------
    data(ExampleData.XSYG, envir = environment())
    bg.spectrum <- set_RLum(class = "RLum.Data.Spectrum", data = TL.Spectrum@data[,15:16, drop = FALSE])

    expect_silent(plot(TL.Spectrum,
                            plot.type="contour",
                            xlim = c(310,750),
                            ylim = c(0,300)))

    expect_silent(suppressWarnings(plot_RLum.Data.Spectrum(TL.Spectrum,
                            plot.type="persp",
                            xlim = c(310,750),
                            ylim = c(0,100),
                            bin.rows=10,
                            bin.cols = 1)))

    ##test background subtraction
    expect_warning(plot_RLum.Data.Spectrum(
      TL.Spectrum,
      plot.type = "persp",
      xlim = c(310, 750),
      ylim = c(0, 300),
      bg.spectrum = bg.spectrum,
      bg.channels = 1:3,
      bin.rows = 10,
      bin.cols = 1
    ))

    ##test energy axis
    expect_silent(plot_RLum.Data.Spectrum(
      TL.Spectrum,
      plot.type = "multiple.lines",
      xlim = c(1.4, 4),
      ylim = c(0, 300),
      bg.spectrum = bg.spectrum,
      bg.channels = 2,
      bin.cols = 1,
      xaxis.energy = TRUE
    ))

   expect_silent(suppressWarnings(plot_RLum.Data.Spectrum(TL.Spectrum,
                            plot.type="multiple.lines",
                            xlim = c(310,750),
                            ylim = c(0,100),
                            bin.rows=10,
                            bin.cols = 1)))

   expect_silent(suppressWarnings(plot_RLum.Data.Spectrum(TL.Spectrum, plot.type="interactive",
                           xlim = c(310,750), ylim = c(0,300), bin.rows=10,
                           bin.cols = 1)))


   expect_silent(suppressWarnings(plot_RLum.Data.Spectrum(TL.Spectrum, plot.type="interactive",
                           xlim = c(310,750), ylim = c(0,300), bin.rows=10,
                           bin.cols = 1,
                           type = "heatmap",
                           showscale = TRUE)))

   expect_silent(suppressWarnings(plot_RLum.Data.Spectrum(TL.Spectrum, plot.type="interactive",
                                                          xlim = c(310,750), ylim = c(0,300), bin.rows=10,
                                                          bin.cols = 1,
                                                          type = "contour",
                                                          showscale = TRUE)))

   expect_error(plot(TL.Spectrum,
                      plot.type="contour",
                      xlim = c(310,750),
                      ylim = c(0,300), bin.cols = 0))



})


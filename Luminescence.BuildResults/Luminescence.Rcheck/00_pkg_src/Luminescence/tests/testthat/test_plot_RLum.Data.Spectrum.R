context("Test Various Plot Functions")


test_that("test pure success of the plotting without warning or error", {
  testthat::skip_on_cran()

    ##RLum.Data.Spectrum -------
    data(ExampleData.XSYG, envir = environment())
    m <- TL.Spectrum@data
    bg.spectrum <- set_RLum(class = "RLum.Data.Spectrum", data = TL.Spectrum@data[,15:16, drop = FALSE])

    ##crash the function with wrong input
    expect_error(plot_RLum.Data.Spectrum(object = "test"),
                 regexp = "Input object neither of class 'RLum.Data.Spectrum' nor 'matrix'.")

    ##try a matrix as input
    expect_message(plot_RLum.Data.Spectrum(object = m),
                   regexp = "Input has been converted to a RLum.Data.Spectrum object using set_RLum()")

    ##remove rownames and column names
    rownames(m) <- NULL
    colnames(m) <- NULL
    expect_message(plot_RLum.Data.Spectrum(object = m),
        regexp = "Input has been converted to a RLum.Data.Spectrum object using set_RLum()")


    ##standard plot with some settings
    expect_silent(plot(
      TL.Spectrum,
      plot.type = "contour",
      main = "Test",
      xlab = "test",
      ylab = "test",
      mtext = "test",
      cex = 1.2,
      pch = 2,
      lwd = 2,
      bty = "n",
      sub = "est"
    ))

    ##persp plot
    expect_silent(suppressWarnings(
      plot_RLum.Data.Spectrum(
        TL.Spectrum,
        plot.type = "persp",
        xlim = c(310, 750),
        ylim = c(0, 100),
        bin.rows = 10,
        bin.cols = 2
      )
    ))

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

    ##multiple.lines
    expect_silent(suppressWarnings(
      plot_RLum.Data.Spectrum(
        TL.Spectrum,
        plot.type = "multiple.lines",
        xlim = c(310, 750),
        ylim = c(0, 100),
        bin.rows = 10,
        bin.cols = 1
      )
    ))

    ##interactive
    expect_silent(suppressWarnings(
      plot_RLum.Data.Spectrum(
        TL.Spectrum,
        plot.type = "interactive",
        xlim = c(310, 750),
        ylim = c(0, 300),
        bin.rows = 10,
        bin.cols = 1
      )
    ))


    ##interactive heatmap
    expect_silent(suppressWarnings(
      plot_RLum.Data.Spectrum(
        TL.Spectrum,
        plot.type = "interactive",
        xlim = c(310, 750),
        ylim = c(0, 300),
        bin.rows = 10,
        bin.cols = 1,
        type = "heatmap",
        showscale = TRUE
      )
    ))

    ##interactive contour
    expect_silent(suppressWarnings(
      plot_RLum.Data.Spectrum(
        TL.Spectrum,
        plot.type = "interactive",
        xlim = c(310, 750),
        ylim = c(0, 300),
        bin.rows = 10,
        bin.cols = 1,
        type = "contour",
        showscale = TRUE
      )
    ))


    ##create more error
    expect_error(plot(
      TL.Spectrum,
      plot.type = "contour",
      xlim = c(310, 750),
      ylim = c(0, 300),
      bin.cols = 0
    ))



})


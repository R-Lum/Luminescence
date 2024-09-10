data(ExampleData.XSYG, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_RLum.Data.Spectrum("error"),
               "'object' must be of type 'RLum.Data.Spectrum' or 'matrix'")
  expect_error(plot_RLum.Data.Spectrum(TL.Spectrum, plot.type = "error"),
               "Unknown plot type")
  expect_error(plot_RLum.Data.Spectrum(TL.Spectrum, bg.spectrum = "error"),
               "Input for 'bg.spectrum' not supported")
  expect_error(plot_RLum.Data.Spectrum(TL.Spectrum, bin.cols = 0),
               "'bin.cols' and 'bin.rows' have to be > 1")

  expect_warning(plot_RLum.Data.Spectrum(TL.Spectrum, bg.channels = -2),
                 "'bg.channels' out of range")
})

test_that("check functionality", {
  testthat::skip_on_cran()

    ##RLum.Data.Spectrum -------
    m <- TL.Spectrum@data
    bg.spectrum <- set_RLum(class = "RLum.Data.Spectrum", data = TL.Spectrum@data[,15:16, drop = FALSE])


    ##try a matrix as input
    expect_message(plot_RLum.Data.Spectrum(object = m),
                   regexp = "Input has been converted to a RLum.Data.Spectrum object using set_RLum()")

    ##remove rownames and column names
    rownames(m) <- NULL
    colnames(m) <- NULL
    expect_message(plot_RLum.Data.Spectrum(object = m),
        regexp = "Input has been converted to a RLum.Data.Spectrum object using set_RLum()")

    ## test duplicated column names
    t <- TL.Spectrum
    colnames(t@data) <- rep(50, ncol(t@data))
    expect_warning(plot_RLum.Data.Spectrum(t),
                   "Duplicated column names found")

    ##standard plot with some settings
    expect_silent(plot_RLum.Data.Spectrum(
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

    ##no plot
    expect_type(plot(
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
      plot = FALSE,
    ), "double")

    ##persp plot
    expect_silent(suppressWarnings(
      plot_RLum.Data.Spectrum(
        TL.Spectrum,
        plot.type = "persp",
        xlim = c(310, 750),
        ylim = c(0, 100),
        bin.rows = 10,
        bin.cols = 2,
        zlab = "test")
    ))

    ##test background subtraction
    expect_warning(plot_RLum.Data.Spectrum(
      TL.Spectrum,
      plot.type = "persp",
      xlim = c(310, 750),
      ylim = c(0, 300),
      bg.spectrum = bg.spectrum,
      bin.rows = 10,
      bin.cols = 1
    ), "6 channels removed due to row \\(wavelength\\) binning")

    ## check output and limit counts
    expect_type(suppressWarnings(plot_RLum.Data.Spectrum(
      TL.Spectrum,
      plot.type = "persp",
      xlim = c(310, 750),
      limit_counts = 10000,
      bg.spectrum = bg.spectrum,
      bin.rows = 10,
      bin.cols = 1
    )), "double")

    ## check our axes
    expect_type(suppressWarnings(plot_RLum.Data.Spectrum(
    TL.Spectrum,
    plot.type = "persp",
    xlim = c(310, 750),
    limit_counts = 10000,
    bg.spectrum = bg.spectrum,
    bin.rows = 10,
    box = "alternate",
    bin.cols = 1
    )), "double")

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

    ## plot: multiple.lines ---------
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

    expect_silent(suppressWarnings(
      plot_RLum.Data.Spectrum(
        TL.Spectrum,
        plot.type = "multiple.lines",
        xlim = c(310, 750),
        frames = c(1,3),
        ylim = c(0, 100),
        bin.rows = 10,
        bin.cols = 1
      )
    ))

    ## plot: image ------------
    ### plot_image: standard -------
    expect_silent(suppressWarnings(
      plot_RLum.Data.Spectrum(
        TL.Spectrum,
        plot.type = "image",
        xlim = c(310, 750),
        ylim = c(0, 300),
        bin.rows = 10,
        bin.cols = 1
      )
    ))

  ### plot_image: no contour -------
   expect_silent(suppressWarnings(
      plot_RLum.Data.Spectrum(
        TL.Spectrum,
        plot.type = "image",
        xlim = c(310, 750),
        ylim = c(0, 300),
        bin.rows = 10,
        bin.cols = 1,
        contour = FALSE
      )))

    ## plot: transect ------------
    expect_silent(suppressWarnings(
      plot_RLum.Data.Spectrum(
        TL.Spectrum,
        plot.type = "transect",
        xlim = c(310, 750),
        ylim = c(0, 300),
        bin.rows = 10,
        bin.cols = 1,
        contour = FALSE)))

    ## plot: single ------------
    expect_silent(suppressWarnings(
      plot_RLum.Data.Spectrum(
        TL.Spectrum,
        plot.type = "single",
        xlim = c(310, 750),
        ylim = c(0, 300),
        bin.rows = 10,
        bin.cols = 6,
        contour = FALSE)))

    ## test frames
    expect_silent(suppressWarnings(
      plot_RLum.Data.Spectrum(
        TL.Spectrum,
        plot.type = "single",
        xlim = c(310, 750),
        frames = 1,
        ylim = c(0, 300),
        bin.rows = 10,
        bin.cols = 6,
        contour = FALSE)))

    ### plot_image: colour changes -------
    expect_silent(suppressWarnings(
      plot_RLum.Data.Spectrum(
        TL.Spectrum,
        plot.type = "image",
        xlim = c(310, 750),
        ylim = c(0, 300),
        bin.rows = 10,
        bin.cols = 1,
        col = grDevices::hcl.colors(20),
        contour.col = "yellow"
      )
    ))

    ## plot: interactive ------------
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

    ## plot: interactive heatmap --------
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

  ## more coverage
  plot_RLum.Data.Spectrum(TL.Spectrum, plot.type = "multiple.lines",
                          phi = 15, theta = -30, r = 10, log = "xyz",
                          shade = 0.4, expand = 0.5, border = 1,
                          axes = FALSE, norm = "min", col = 2)
})

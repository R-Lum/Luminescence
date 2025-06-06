## load data
data(ExampleData.XSYG, envir = environment())
bg.spectrum <- set_RLum("RLum.Data.Spectrum",
                        data = TL.Spectrum@data[, 15:16, drop = FALSE])

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_RLum.Data.Spectrum("error"),
               "'object' should be of class 'RLum.Data.Spectrum' or 'matrix'")
  expect_error(plot_RLum.Data.Spectrum(set_RLum("RLum.Data.Spectrum")),
               "'object' contains no data")
  expect_error(plot_RLum.Data.Spectrum(TL.Spectrum, plot.type = "error"),
               "'plot.type' should be one of 'contour', 'persp', 'single'")
  expect_error(plot_RLum.Data.Spectrum(TL.Spectrum, norm = "error"),
               "'norm' should be one of 'min', 'max' or NULL")
  expect_error(plot_RLum.Data.Spectrum(TL.Spectrum, bg.spectrum = "error"),
               "'bg.spectrum' should be of class 'RLum.Data.Spectrum' or 'matrix'")
  expect_error(plot_RLum.Data.Spectrum(TL.Spectrum, bin.rows = 1.7),
               "'bin.rows' should be a positive integer scalar")
  expect_error(plot_RLum.Data.Spectrum(TL.Spectrum, bin.cols = 0),
               "'bin.cols' should be a positive integer scalar")

  expect_error(plot_RLum.Data.Spectrum(TL.Spectrum, xlim = c(0, 100)),
      "No data left after applying 'xlim' and 'ylim'")
  expect_error(plot_RLum.Data.Spectrum(TL.Spectrum, ylim = c(5, 10)),
      "No data left after applying 'xlim' and 'ylim'")

  expect_warning(plot_RLum.Data.Spectrum(TL.Spectrum, bg.channels = -2),
                 "'bg.channels' out of range")
})

test_that("check functionality", {
  testthat::skip_on_cran()

    ##RLum.Data.Spectrum -------
    m <- TL.Spectrum@data

    ##try a matrix as input
    expect_message(plot_RLum.Data.Spectrum(object = m, xaxis.energy = TRUE),
                   "Input has been converted to a 'RLum.Data.Spectrum' object")

    ##remove rownames and column names
    rownames(m) <- NULL
    colnames(m) <- NULL
    expect_message(plot_RLum.Data.Spectrum(object = m),
        regexp = "Input has been converted to a 'RLum.Data.Spectrum' object")

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

    ##test background subtraction ... with bgchannel
    expect_warning(plot_RLum.Data.Spectrum(
      TL.Spectrum,
      plot.type = "persp",
      xlim = c(310, 750),
      ylim = c(0, 300),
      bg.spectrum = bg.spectrum,
      bg.channels = 1,
      plot = FALSE,
      bin.rows = 10,
      bin.cols = 1
    ), "6 channels removed due to row \\(wavelength\\) binning")

    ## check output and limit counts
    expect_warning(plot_RLum.Data.Spectrum(
      TL.Spectrum,
      plot.type = "persp",
      xlim = c(310, 750),
      limit_counts = 2,
      lphi = 20,
      ltheta = -15,
      ticktype = "simple",
      bin.rows = 1,
      bin.cols = 1
    ), "Lowest count value is larger than the set count threshold")

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

  expect_silent(suppressWarnings(
      plot_RLum.Data.Spectrum(
        TL.Spectrum,
        plot.type = "transect",
        xlim = c(310, 750),
        ylim = c(0, 350),
#        bin.rows = 50,
        ## bin.cols = 1,
        zlim = c(0, 1e6),
        ylab = "Counts [1 / summed channels]",
        contour = TRUE)))

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
        col = 1,
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
        norm = "max",
        showscale = TRUE
      )
    ))

  ## more coverage
  plot_RLum.Data.Spectrum(TL.Spectrum, plot.type = "multiple.lines",
                          phi = 15, theta = -30, r = 10, log = "xyz",
                          shade = 0.4, expand = 0.5, border = 1,
                          axes = FALSE, norm = "min", col = 2, zlim = c(0, 2))

  expect_message(expect_null(
      plot_RLum.Data.Spectrum(TL.Spectrum,
                              plot.type = "multiple.lines",
                              xlim = c(1.4, 4), ylim = c(0, 300),
                              bg.spectrum = bg.spectrum@data * 2,
                              xaxis.energy = TRUE)),
      "Error: After background subtraction all counts are negative")

  spec <- TL.Spectrum
  rownames(spec@data) <- colnames(spec@data) <- NULL
  rownames(bg.spectrum@data) <- NULL
  expect_silent(plot_RLum.Data.Spectrum(spec, bg.spectrum = bg.spectrum,
                                        xlim = c(0, 300), ylim = c(0, 500)))
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not(getRversion() >= "4.4.0")

  SW({
  vdiffr::expect_doppelganger("contour",
                              plot_RLum.Data.Spectrum(TL.Spectrum,
                                                      plot.type = "contour",
                                                      ylim = c(0, 200),
                                                      cex = 2))
  vdiffr::expect_doppelganger("persp",
                              plot_RLum.Data.Spectrum(TL.Spectrum,
                                                      plot.type = "persp",
                                                      xlim = c(310, 750),
                                                      limit_counts = 10000,
                                                      bg.spectrum = bg.spectrum,
                                                      bin.rows = 10,
                                                      bin.cols = 1))
  vdiffr::expect_doppelganger("single",
                              plot_RLum.Data.Spectrum(TL.Spectrum,
                                                      plot.type = "single",
                                                      xlim = c(310, 750),
                                                      ylim = c(0, 300),
                                                      bin.cols = 10))
  vdiffr::expect_doppelganger("multiple",
                              plot_RLum.Data.Spectrum(TL.Spectrum,
                                                      plot.type = "multiple.lines",
                                                      xlim = c(1.4, 4),
                                                      ylim = c(0, 300),
                                                      bg.spectrum = bg.spectrum,
                                                      bg.channels = 2,
                                                      bin.cols = 1,
                                                      xaxis.energy = TRUE))
  })
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 415
  expect_silent(plot_RLum.Data.Spectrum(
      TL.Spectrum,
      ylim = c(0, 100),
      bin.cols = 3))
  expect_warning(plot_RLum.Data.Spectrum(
      TL.Spectrum,
      ylim = c(0, 100),
      bin.cols = 8),
      "Single column matrix: plot.type has been automatically reset to")
  expect_silent(plot_RLum.Data.Spectrum(
      TL.Spectrum,
      bin.rows = 600))
  expect_message(expect_null(plot_RLum.Data.Spectrum(
      TL.Spectrum,
      bin.rows = 2000)),
      "Insufficient data for plotting, NULL returned")

  ## issue 726
  spec <- TL.Spectrum
  rownames(spec@data) <- colnames(spec@data) <- NULL
  bg.spectrum <- set_RLum(class = "RLum.Data.Spectrum",
                          data = spec@data[, 15:16, drop = FALSE])
  expect_silent(plot_RLum.Data.Spectrum(spec, bg.spectrum = bg.spectrum,
                                        xlim = c(0, 100), ylim = c(0, 10)))
})

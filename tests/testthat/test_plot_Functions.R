test_that("test pure success of the plotting without warning or error", {
  testthat::skip_on_cran()

  ##distribution plots
  data(ExampleData.DeValues, envir = environment())
  ExampleData.DeValues <- ExampleData.DeValues$CA1

  expect_silent(plot_RadialPlot(ExampleData.DeValues))
  expect_silent(plot_KDE(ExampleData.DeValues))

   ##plot_Det
  data(ExampleData.BINfileData, envir = environment())
  object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)
  expect_s4_class(
    plot_DetPlot(
      object,
      signal.integral.min = 1,
      signal.integral.max = 3,
      background.integral.min = 900,
      background.integral.max = 1000,
      n.channels = 5,
      verbose = FALSE,
    ),
    "RLum.Results"
  )

  ##various RLum plots

    ##RLum.Data.Curve
    data(ExampleData.CW_OSL_Curve, envir = environment())
    temp <- as(ExampleData.CW_OSL_Curve, "RLum.Data.Curve")
    expect_silent(plot(temp))

    ##RLum.Data.Spectrum -------
    data(ExampleData.XSYG, envir = environment())
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


    ##RLum.Analysis
    data(ExampleData.BINfileData, envir = environment())
    temp <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)
    expect_silent(plot(
      temp,
      subset = list(recordType = "TL"),
      combine = TRUE,
      norm = TRUE,
      abline = list(v = c(110))
    ))

    ##special plot RLum.Reuslts
    data(ExampleData.DeValues, envir = environment())
    mam <- calc_MinDose(data = ExampleData.DeValues$CA1, sigmab = 0.2,
                        log = TRUE, plot = FALSE, verbose = FALSE)
    expect_silent(plot_RLum(mam))
    cdm <- calc_CentralDose(ExampleData.DeValues$CA1, verbose = FALSE)
    expect_silent(plot_RLum(cdm))
    FMM <- calc_FiniteMixture(ExampleData.DeValues$CA1, verbose = FALSE,
                             sigmab = 0.2, n.components = c(2:4),
                             pdf.weight = TRUE, dose.scale = c(0, 100))
    plot_RLum(FMM)
})

test_that("test for return values, if any", {
  testthat::skip_on_cran()

  data(ExampleData.DeValues, envir = environment())
  output <- plot_AbanicoPlot(ExampleData.DeValues, output = TRUE)
    expect_type(output, "list")
    expect_length(output, 10)
})

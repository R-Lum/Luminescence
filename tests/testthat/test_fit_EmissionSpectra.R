## load data
data(ExampleData.XSYG, envir = environment())

## subtract background
TL.Spectrum@data <- TL.Spectrum@data[] - TL.Spectrum@data[, 15]

## matrix input
mat <- get_RLum(TL.Spectrum)[, 1:4]

## spectrum with only 1 frame
TL1 <- set_RLum("RLum.Data.Spectrum", data = TL.Spectrum[, 1, drop = FALSE])

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(fit_EmissionSpectra("fail"),
               "'object' should be of class 'RLum.Data.Spectrum', 'matrix' or")
  expect_error(fit_EmissionSpectra(list(TL.Spectrum, "fail")),
               "[fit_EmissionSpectra()] List elements of different class detected",
               fixed = TRUE)
  expect_error(fit_EmissionSpectra(matrix()),
               "'object' should have at least two columns")
  expect_error(fit_EmissionSpectra(TL.Spectrum, n_components = 1.4),
               "'n_components' should be a positive integer scalar")
  expect_error(fit_EmissionSpectra(TL.Spectrum, input_scale = "error"),
               "'input_scale' should be one of 'wavelength', 'energy' or NULL")
  expect_error(fit_EmissionSpectra(TL.Spectrum, method_control = "error"),
               "'method_control' should be of class 'list'")
  expect_error(fit_EmissionSpectra(TL.Spectrum, verbose = "error"),
               "'verbose' should be a single logical value")
  expect_error(fit_EmissionSpectra(TL.Spectrum, plot = "error"),
               "'plot' should be a single logical value")

  ## wrong frame range -------
  expect_error(fit_EmissionSpectra(TL.Spectrum, frame = "error"),
               "'frame' should be of class 'integer', 'numeric' or NULL")
  expect_error(fit_EmissionSpectra(TL.Spectrum, frame = 1000),
               "Invalid 'frame', allowed values range from 1 to 24")

  ## wrong graining argument -------
  SW({
  expect_error(fit_EmissionSpectra(TL.Spectrum, frame = 5,
      method_control = list(graining = 10000)),
      "method_control$graining cannot exceed the available channels (1024)",
      fixed = TRUE)
  })

  ## for matrix input -------
  expect_error(fit_EmissionSpectra(object = mat, frame = "error"),
               "'frame' should be of class 'integer', 'numeric' or NULL")
  expect_error(fit_EmissionSpectra(object = mat, frame = 1000),
               "Invalid 'frame', allowed values range from 1 to 3")

  ## empty object
  expect_error(fit_EmissionSpectra(set_RLum("RLum.Data.Spectrum")),
               "'object' contains no data")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## no frame specified
  results <- fit_EmissionSpectra(
      object = TL1,
      plot = FALSE,
      verbose = FALSE,
      method_control = list(max.runs = 5))
  expect_equal(results@data$df_plot[[1]], NA)
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 5.0e-1

  SW({
  set.seed(1)
  expect_snapshot_RLum(fit_EmissionSpectra(
    object = TL.Spectrum,
    frame = 5,
    n_components = 3,
    plot = FALSE,
    start_parameters = c(2.17),
    method_control = list(max.runs = 20,
                          export.plot.data = TRUE)),
    tolerance = snapshot.tolerance)

  ## input_scale
  set.seed(1)
  expect_snapshot_RLum(
      fit_EmissionSpectra(object = TL.Spectrum, frame = 5,
                          input_scale = "wavelength", plot = FALSE,
                          method_control = list(max.runs = 5)),
      tolerance = snapshot.tolerance)
  })

  ## for matrix input -------
  set.seed(17)
  expect_snapshot_RLum(
      fit_EmissionSpectra(object = mat, plot = FALSE, verbose = FALSE,
                          method_control = list(max.runs = 5)),
      tolerance = snapshot.tolerance)
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  SW({
  set.seed(1)
  vdiffr::expect_doppelganger("3 comps",
                              fit_EmissionSpectra(TL.Spectrum,
                                                  frame = 5,
                                                  n_components = 3,
                                                  main = "TL spectrum",
                                                  mtext = "Subtitle",
                                                  verbose = FALSE,
                                                  method_control = list(max.runs = 5)))
  set.seed(1)
  vdiffr::expect_doppelganger("wavelength",
                              fit_EmissionSpectra(TL.Spectrum,
                                                  frame = 5,
                                                  n_components = 2,
                                                  input_scale = "wavelength",
                                                  verbose = FALSE,
                                                  method_control = list(max.runs = 5)))
   vdiffr::expect_doppelganger("control plot",
                              fit_EmissionSpectra(TL.Spectrum,
                                                  frame = 4,
                                                  n_components = 2,
                                                  verbose = FALSE,
                                                  method_control = list(max.runs = 5)))
  })
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 761
  expect_silent(fit_EmissionSpectra(TL.Spectrum, frame = 1, plot = TRUE,
                                    n_components = 2, verbose = FALSE,
                                    method_control = list(max.runs = 5)))

  ## issue 932
  expect_error(fit_EmissionSpectra(matrix(rnorm(5), 1, 5), verbose = FALSE),
               "method_control$graining cannot exceed the available channels",
               fixed = TRUE)
})

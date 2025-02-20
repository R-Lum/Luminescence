## load data
data(ExampleData.XSYG, envir = environment())

## subtract background
TL.Spectrum@data <- TL.Spectrum@data[] - TL.Spectrum@data[, 15]

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
  mat <- get_RLum(TL.Spectrum)[, 1:4]
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

SW({

  ## for matrix input -------
  mat <- get_RLum(TL.Spectrum)[, 1:4]
  expect_s4_class(
      fit_EmissionSpectra(object = mat, plot = FALSE, verbose = FALSE,
                          method_control = list(max.runs = 5)),
      "RLum.Results")
  })

  # plain run -------
  SW({
  results <-  expect_s4_class(fit_EmissionSpectra(
    object = TL.Spectrum,
    frame = 5,
    main = "TL spectrum",
    n_components = 3,
    plot = TRUE,
    start_parameters = c(2.17),
    method_control = list(max.runs = 100,
                          export.plot.data = TRUE)), "RLum.Results")
  })
  expect_length(results, 4)
  expect_s3_class(results$fit[[1]], "nls")
  expect_type(results$data, "double")
  expect_length(results@data$df_plot, 1)
  expect_named(results@data$df_plot[[1]],
               c("ENERGY", "WAVELENGTH", "SUM", "COMP_1", "COMP_2", "COMP_3"))

  # silent mode -------
  expect_silent(results <- fit_EmissionSpectra(
    object = TL.Spectrum,
    frame = 5,
    main = "TL spectrum",
    plot = FALSE,
    verbose = FALSE,
    method_control = list(max.runs = 10)))
  expect_equal(results@data$df_plot[[1]], NA)

  ## input_scale
  SW({
  expect_s4_class(
      fit_EmissionSpectra(object = TL.Spectrum, frame = 5,
                          input_scale = "wavelength", plot = FALSE,
                          method_control = list(max.runs = 5)),
      "RLum.Results")
  })

  ## plot
  set.seed(1)
  SW({
  expect_s4_class(
      fit_EmissionSpectra(object = TL.Spectrum, frame = 5, plot = TRUE,
                          n_components = 3, verbose = FALSE, mtext = "Subtitle",
                          method_control = list(max.runs = 5)),
      "RLum.Results")
  })
})

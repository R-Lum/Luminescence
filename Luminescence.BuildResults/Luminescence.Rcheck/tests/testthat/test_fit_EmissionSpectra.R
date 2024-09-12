test_that("standard check", {
  testthat::skip_on_cran()

  ##load example data
  data(ExampleData.XSYG, envir = environment())

  ##subtract background
  TL.Spectrum@data <- TL.Spectrum@data[] - TL.Spectrum@data[,15]

  # break function -----------
  ## unwanted list element in list ---------
  expect_error(fit_EmissionSpectra(list(TL.Spectrum, "fail")),
               "\\[fit\\_EmissionSpectra\\(\\)\\] List elements of different class detected!")

  ## wrong frame range -------
  expect_error(fit_EmissionSpectra(TL.Spectrum, frame = 1000),
               "\\[fit\\_EmissionSpectra\\(\\)\\] 'frame' invalid. Allowed range min: 1 and max: 24")


  ## wrong graining argument -------
  SW({
  expect_error(fit_EmissionSpectra(TL.Spectrum, frame = 5,
      method_control = list(graining = 10000)),
      "method_control$graining cannot exceed the available channels (1024)",
      fixed = TRUE)
  })

  ## for matrix input -------
  expect_error(fit_EmissionSpectra("fail"),
               "Objects of type 'character' are not supported")

  mat <- get_RLum(TL.Spectrum)[, 1:4]
  expect_error(fit_EmissionSpectra(object = mat, frame = 1000),
               "'frame' invalid. Allowed range min: 1 and max: 3")
  SW({
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
    method_control = list(max.runs = 100)), "RLum.Results")
  })

  # silent mode -------
  expect_silent(fit_EmissionSpectra(
    object = TL.Spectrum,
    frame = 5,
    main = "TL spectrum",
    plot = FALSE,
    verbose = FALSE,
    method_control = list(max.runs = 10)))

 # regression test ----
 expect_length(results, 3)
 expect_s3_class(results$fit[[1]], "nls")
 expect_type(results$data, "double")

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

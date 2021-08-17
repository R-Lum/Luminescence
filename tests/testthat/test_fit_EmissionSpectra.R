test_that("standard check", {
  testthat::skip_on_cran()
  local_edition(3)

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
  expect_error(fit_EmissionSpectra(TL.Spectrum, frame = 5,
      method_control = list(graining = 10000)),
      "\\[fit\\_EmissionSpectra\\(\\)\\] method\\_control\\$graining cannot be larger than available channels \\(1024\\)!")

  ## for matrix input -------
  expect_error(fit_EmissionSpectra("fail"),
               "\\[fit\\_EmissionSpectra\\(\\)\\] Input not supported, please read the manual!")


  # plain run -------
  set.seed(1235)
  results <- expect_s4_class(fit_EmissionSpectra(
    object = TL.Spectrum,
    frame = 5, main = "TL spectrum",
    method_control = list(max.runs = 100)), "RLum.Results")



})


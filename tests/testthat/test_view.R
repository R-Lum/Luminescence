## load data
data(ExampleData.RLum.Analysis)
data(ExampleData.RLum.Data.Image, envir = environment())
data(ExampleData.XSYG, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(view(set_RLum("RLum.Data.Image")),
               "'info' slot cannot be an empty list")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## Risø.BINfileData
  file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
  risoe <- read_BIN2R(file, verbose = FALSE)

  ## RLum.Data.Curve
  curve <- get_RLum(OSL.SARMeasurement$Sequence.Object,
                    recordType = "TL (UVVIS)")[[1]]

  ## RLum.Data.Image
  image <- ExampleData.RLum.Data.Image

  ## RLum.Data.Spectrum
  spect <- TL.Spectrum

  ## RLum.Results
  results <- calc_FastRatio(curve, verbose = FALSE, plot = FALSE)

  ## RLum.Analysis
  analysis <- OSL.SARMeasurement[[2]]

  ## we need to mock the utils::View function as it otherwise generates this
  ## error on CI:
  ##   Error in `check_screen_device("View()")`:
  ##     View() should not be used in examples etc
  with_mocked_bindings(".view" = function(x, ...) NULL, {
    expect_null(view(risoe))
    expect_null(view(curve))
    expect_null(view(image))
    expect_null(view(spect))
    expect_null(view(results))
    expect_null(view(analysis))
  })
})

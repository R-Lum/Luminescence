## set the path for the files
IRSL <- system.file("extdata/BINX_IRSL_LOG.TXT", package = "Luminescence")
OSL <- system.file("extdata/BINX_OSL_LOG.TXT", package = "Luminescence")
IRSL_BROKEN <- system.file("extdata/BINX_IRSL_LOG_BROKEN.TXT", package = "Luminescence")

test_that("input validation", {
  testthat::skip_on_cran()

  ## nothing for file
  expect_error(read_BINXLOG2R(), regexp = "'file' should be of class 'character' or 'list'")

  ## wrong file path
  expect_error(read_BINXLOG2R(file = "error"),
               "File '.*error' does not exist") # windows CI needs the regexp

  ## verbose wrong
  expect_error(read_BINXLOG2R(file = IRSL, verbose = "error"), regexp = "'verbose' should be a single logical value")

  ## broken file
  expect_error(read_BINXLOG2R(file = IRSL_BROKEN, verbose = FALSE), regexp = "The file does not seem to be a valid debugging log-file")
})

test_that("standard run", {
  testthat::skip_on_cran()

  ## simple input
  SW ({
  t <- expect_type(read_BINXLOG2R(file = IRSL), "list")
  expect_length(t, 1)
  expect_s4_class(t[[1]], "RLum.Analysis")
  })

  ## IRSL verbose false
  expect_type(read_BINXLOG2R(file = IRSL, verbose = FALSE), "list")

  ## OSL verbose false
  expect_type(read_BINXLOG2R(file = OSL, verbose = FALSE), "list")

  ## list verbose false
  t <- expect_type(read_BINXLOG2R(file = list(IRSL, OSL), verbose = FALSE), "list")
  expect_length(t, 2)
  expect_s4_class(t[[1]], "RLum.Analysis")

  ## check whether we can export
  expect_silent(Luminescence::write_R2BIN(
    object = Luminescence::convert_RLum2Risoe.BINfileData(t),
    file = tempfile(),
    version = 4,
    verbose = FALSE))
})

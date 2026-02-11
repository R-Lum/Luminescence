## path to the SPE file on github, as it's not included in the package
github.url <- file.path("https://github.com/R-Lum/Luminescence",
                        "raw/master/tests/testthat/_data")
local.file <- test_path("_data/SPEfile.SPE")

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(read_SPE2R(data.frame()),
               "'file' should be of class 'character'")
  expect_error(read_SPE2R(character(0)),
               "'file' should have length 1")
  expect_error(read_SPE2R("error"),
               "File '.*error' does not exist") # windows CI needs the regexp
  expect_error(read_SPE2R(local.file, output.object = "error"),
               "'output.object' should be one of 'RLum.Data.Image'")
  expect_error(read_SPE2R(local.file, frame.range = NA),
               "'frame.range' should be of class 'integer', 'numeric' or NULL")
  expect_error(read_SPE2R(local.file, frame.range = c(2, NA)),
               "'frame.range' should contain positive values")
  expect_error(read_SPE2R(local.file, frame.range = c(0, 3)),
               "'frame.range' should contain positive values")
  expect_message(expect_message(expect_null(read_SPE2R("http://httpbingo.org/status/404")),
                                "Downloading"), "FAILED")

  wrong <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
  expect_error(read_SPE2R(wrong),
               "File extension 'binx' is not supported, only 'SPE' is valid")
  expect_message(expect_message(expect_null(read_SPE2R(dirname(wrong))),
                                "Directory detected, looking for"),
                 "Error: No files matching the given pattern found in directory")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## default values
  SW({
  expect_message(
      expect_s4_class(read_SPE2R(file.path(github.url, "SPEfile.SPE")),
                      "RLum.Data.Image"),
      "Downloading")
  })
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6

  expect_snapshot_RLum(read_SPE2R(local.file,
                                  output.object = "RLum.Data.Spectrum",
                                  frame.range = 20:40,
                                  verbose = FALSE),
                       tolerance = snapshot.tolerance)
  expect_snapshot_plain(read_SPE2R(local.file,
                                   output.object = "matrix",
                                   verbose = FALSE),
                        tolerance = snapshot.tolerance)
})

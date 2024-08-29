## path to the SPE file on github, as it's not included in the package
github.url <- file.path("https://github.com/R-Lum/Luminescence",
                        "raw/dev_0.9.x/tests/testthat/_data")

test_that("input validation", {
  testthat::skip_on_cran()

  expect_message(expect_null(read_SPE2R("error")),
                 "Error: File does not exist, NULL returned")
  expect_error(read_SPE2R(file.path(github.url, "SPEfile.SPE"),
                          output.object = "error"),
               "'output.object' not supported, valid options are")

  SW({
  expect_message(expect_null(read_SPE2R("http://httpbun.org/status/404")),
                 "Error: File does not exist, NULL returned")
  })

  wrong <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
  expect_error(read_SPE2R(wrong),
               "Unsupported file format")
})

test_that("Test general functionality", {
  testthat::skip_on_cran()

  ## default values
  SW({
  expect_s4_class(read_SPE2R(file.path(github.url, "SPEfile.SPE")),
                  "RLum.Data.Image")
  })

  ## test output.object
  expect_s4_class(read_SPE2R(file.path(github.url, "SPEfile.SPE"),
                             output.object = "RLum.Data.Spectrum",
                             verbose = FALSE),
                  "RLum.Data.Spectrum")
  ret <- read_SPE2R(file.path(github.url, "SPEfile.SPE"),
                    output.object = "matrix",
                    verbose = FALSE)
  expect_true(is.matrix(ret))
})

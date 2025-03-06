## path to the SPE file on github, as it's not included in the package
github.url <- file.path("https://github.com/R-Lum/Luminescence",
                        "raw/master/tests/testthat/_data")

## `read_SPE2R()` calls `download.file()` which, in turn, uses curl to
## perform the actual download. If `verbose = TRUE`, curl is invoked with
## `quiet = FALSE`, and the output it produces cannot be captured by `SW()`,
## nor by other simple R approaches because curl writes directly to the
## console bypassing R. The workaround is to divert all output to a file, see:
## https://stackoverflow.com/questions/66138345/how-to-suppress-download-file-trying-url-message-in-r
sink.curl.messages <- function(expr) {
  nullcon <- file(nullfile(), open = "wb")
  sink(nullcon, type = "message")
  expr
  sink(type = "message")
  close(nullcon)
}

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(read_SPE2R(data.frame()),
               "'file' should be of class 'character'")
  expect_error(read_SPE2R(character(0)),
               "'file' should have length 1")
  expect_message(expect_null(read_SPE2R("error")),
                 "Error: File does not exist, NULL returned")
  expect_error(read_SPE2R(file.path(github.url, "SPEfile.SPE"),
                          output.object = "error"),
               "'output.object' should be one of 'RLum.Data.Image'")

  SW({
  expect_message(expect_null(read_SPE2R("http://httpbun.org/status/404")),
                 "Error: File does not exist, NULL returned")
  })

  wrong <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
  expect_error(read_SPE2R(wrong),
               "Unsupported file format")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## default values
  sink.curl.messages(
  expect_output(
      expect_s4_class(read_SPE2R(file.path(github.url, "SPEfile.SPE")),
                      "RLum.Data.Image"),
      "URL detected, checking connection")
  )
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6

  expect_snapshot_RLum(read_SPE2R(file.path(github.url, "SPEfile.SPE"),
                                  output.object = "RLum.Data.Spectrum",
                                  verbose = FALSE),
                       tolerance = snapshot.tolerance)
  expect_snapshot_plain(read_SPE2R(file.path(github.url, "SPEfile.SPE"),
                                   output.object = "matrix",
                                   verbose = FALSE),
                        tolerance = snapshot.tolerance)
})

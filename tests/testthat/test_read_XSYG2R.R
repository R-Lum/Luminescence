context("read_XSYG2R")

test_that("test import of XSYG files", {
  testthat::skip_on_cran()

  ##force error
  expect_type(read_XSYG2R("https://raw.githubusercontent.com/R-Lum/rxylib/master/inst/extg", fastForward = TRUE), type = "NULL")
  expect_type(read_XSYG2R("/Test", fastForward = TRUE), type = "NULL")

  ##successfull import
  expect_type(read_XSYG2R("https://raw.githubusercontent.com/R-Lum/rxylib/master/inst/extdata/TLSpectrum.xsyg", import = FALSE), type = "list")
  expect_is(read_XSYG2R("https://raw.githubusercontent.com/R-Lum/rxylib/master/inst/extdata/TLSpectrum.xsyg", fastForward = TRUE, import = FALSE), class = "data.frame")
  expect_silent(read_XSYG2R("https://raw.githubusercontent.com/R-Lum/rxylib/master/inst/extdata/TLSpectrum.xsyg", verbose = FALSE))
  expect_silent(read_XSYG2R("https://raw.githubusercontent.com/R-Lum/rxylib/master/inst/extdata/TLSpectrum.xsyg", verbose = FALSE, recalculate.TL.curves = FALSE))
  expect_silent(read_XSYG2R("https://raw.githubusercontent.com/R-Lum/rxylib/master/inst/extdata/TLSpectrum.xsyg", verbose = FALSE, pattern = "xsyg",recalculate.TL.curves = FALSE))
  expect_type(read_XSYG2R("https://raw.githubusercontent.com/R-Lum/rxylib/master/inst/extdata/TLSpectrum.xsyg", fastForward = FALSE), type = "list")
  results <- expect_type(read_XSYG2R("https://raw.githubusercontent.com/R-Lum/rxylib/master/inst/extdata/TLSpectrum.xsyg", fastForward = TRUE), type = "list")
  expect_output(print(results))

})



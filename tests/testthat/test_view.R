test_that("check class and length of output", {
  testthat::skip_on_cran()

  ## Test Risø.BINfileData
  file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
  temp <- read_BIN2R(file, verbose = FALSE)
  expect_null(view(temp))
})

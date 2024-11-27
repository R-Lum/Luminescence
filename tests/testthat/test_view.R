test_that("check class and length of output", {
  testthat::skip_on_cran()

  ## Test Risø.BINfileData
  file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
  temp <- read_BIN2R(file, verbose = FALSE)

  with_mocked_bindings(
    "view" = function(x, ...) { TRUE },
    {
      expect_true(view(temp))
    }
  )

})

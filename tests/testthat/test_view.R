test_that("check class and length of output", {
  testthat::skip_on_cran()

  ## Test Risø.BINfileData
  file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
  temp <- read_BIN2R(file, verbose = FALSE)

  ## we need to mock the utils::View function as it otherwise generates this
  ## error on CI:
  ##   Error in `check_screen_device("View()")`:
  ##     View() should not be used in examples etc
  with_mocked_bindings(".view" = function(x, ...) NULL, {
    expect_null(view(temp))
  })
})

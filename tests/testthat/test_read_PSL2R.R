context("read_PSL2R")

test_that("Test functionality", {
  testthat::skip_on_cran()
  
  ## default values
  expect_is(read_PSL2R(
    file = system.file("extdata/DorNie_0016.psl", package = "Luminescence")
  ), "RLum.Analysis")
  
  ## custom values (all inverted)
  expect_is(read_PSL2R(
    file = system.file("extdata/DorNie_0016.psl", package = "Luminescence"),
    drop_bg = TRUE, as_decay_curve = FALSE, smooth = TRUE, merge = TRUE
  ), "RLum.Analysis")
  
})

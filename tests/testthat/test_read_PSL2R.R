test_that("Test functionality", {
  testthat::skip_on_cran()
  local_edition(3)

  ## default values
  expect_s4_class(read_PSL2R(
    file = system.file("extdata/DorNie_0016.psl", package = "Luminescence")
  ), "RLum.Analysis")

  ## custom values (all inverted)
  expect_s4_class(read_PSL2R(
    file = system.file("extdata/DorNie_0016.psl", package = "Luminescence"),
    drop_bg = TRUE, as_decay_curve = FALSE, smooth = TRUE, merge = TRUE
  ), "RLum.Analysis")

})

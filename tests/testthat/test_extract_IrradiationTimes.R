test_that("Test the extraction of irradiation times", {
  testthat::skip_on_cran()
  local_edition(3)

  ##general test
  file <- system.file("extdata/XSYG_file.xsyg", package="Luminescence")

  ##general test
  expect_s4_class(extract_IrradiationTimes(object = file, txtProgressBar = FALSE), "RLum.Results")
})



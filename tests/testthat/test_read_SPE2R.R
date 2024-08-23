test_that("Test general functionality", {
  testthat::skip_on_cran()

  github.url <- file.path("https://github.com/R-Lum/Luminescence",
                          "raw/dev_0.9.x/tests/testthat/_data")

  ##crash function
  expect_null(read_SPE2R(file = "text"))

  ## default values
  expect_s4_class(read_SPE2R(file.path(github.url, "SPEfile.SPE")),
                  "RLum.Data.Image")

  ##test verbose
  expect_s4_class(read_SPE2R(file.path(github.url, "SPEfile.SPE"),
                             verbose = FALSE),
                  "RLum.Data.Image")
})

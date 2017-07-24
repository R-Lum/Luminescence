context("RLum")

test_that("check class", {
  testthat::skip_on_cran()

  object <- set_RLum(class = "RLum.Data.Curve")
  expect_length(rep(object, 10), 10)

})

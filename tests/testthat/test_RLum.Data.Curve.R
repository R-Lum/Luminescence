context("RLum.Data.Curve")

test_that("check class", {
  testthat::skip_on_cran()

  showClass("RLum.Data.Curve")

  ##set empty curve object and show it
  expect_output(show(set_RLum(class = "RLum.Data.Curve")))

  ##check replacements fo
  object <- set_RLum(class = "RLum.Data.Curve")
  expect_s4_class(set_RLum(class = "RLum.Data.Curve", data = object), class = "RLum.Data.Curve")

})

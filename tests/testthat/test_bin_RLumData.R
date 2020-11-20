data(ExampleData.CW_OSL_Curve, envir = environment())
curve <-
  set_RLum(
      class = "RLum.Data.Curve",
      recordType = "OSL",
      data = as.matrix(ExampleData.CW_OSL_Curve)
  )


test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_s4_class(bin_RLum.Data(curve), class = "RLum.Data.Curve")
  expect_length(bin_RLum.Data(curve)[,1], 500)

})

test_that("check values from output example", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_equal(sum(bin_RLum.Data(curve)[,2]), 119200)
  expect_equal(sum(bin_RLum.Data(curve, bin = 5)[1,2]), 41146)

})

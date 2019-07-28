context("smooth_RLum")

data(ExampleData.CW_OSL_Curve, envir = environment())
temp <-
  set_RLum(
      class = "RLum.Data.Curve",
      recordType = "OSL",
      data = as.matrix(ExampleData.CW_OSL_Curve)
  )

##create RLum.Analysis object
temp_analysis <- set_RLum("RLum.Analysis", records = list(temp, temp))

test_that("check class and length of output", {
  testthat::skip_on_cran()

  ##standard tests
  expect_is(temp, class = "RLum.Data.Curve", info = NULL, label = NULL)
  expect_is(smooth_RLum(temp), class = "RLum.Data.Curve", info = NULL, label = NULL)

  ##test on a list
    ##RLum list
    expect_is(smooth_RLum(list(temp, temp)), "list")

    ##normal list
    expect_is(smooth_RLum(list(a = 1, b = 2)), "list")

  ##test on an RLum.Analysis-object
  expect_s4_class(smooth_RLum(temp_analysis), "RLum.Analysis")

})

test_that("check values from output example", {
 testthat::skip_on_cran()
 expect_equivalent(round(mean(smooth_RLum(temp, k = 5)[,2], na.rm = TRUE), 0), 100)
 expect_equivalent(round(mean(smooth_RLum(temp, k = 10)[,2], na.rm = TRUE), 0), 85)

})

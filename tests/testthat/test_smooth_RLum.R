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
  expect_s4_class(temp, class = "RLum.Data.Curve")
  expect_snapshot_RLum(smooth_RLum(temp))

  ##test on a list
    ##RLum list
    expect_type(smooth_RLum(list(temp, temp)), "list")

    ##normal list
    expect_type(smooth_RLum(list(a = 1, b = 2)), "list")

  ##test on an RLum.Analysis-object
  expect_s4_class(smooth_RLum(temp_analysis), "RLum.Analysis")

})

test_that("snapshot tests", {
 testthat::skip_on_cran()

 small <-set_RLum(class = "RLum.Data.Curve", recordType = "OSL",
                  data = as.matrix(ExampleData.CW_OSL_Curve[1:150, ]))
 expect_snapshot_RLum(smooth_RLum(small, k = 5))
 expect_snapshot_RLum(smooth_RLum(small, k = 10))
 expect_snapshot_RLum(smooth_RLum(small, k = 11, method = "median"))
})

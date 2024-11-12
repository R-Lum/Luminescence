test_that("check functionality", {
  testthat::skip_on_cran()

  data(ExampleData.BINfileData, envir = environment())

  res <- .Risoe.BINfileData2RLum.Data.Curve(CWOSL.SAR.Data, id = 1)
  expect_s4_class(res, "RLum.Data.Curve")
  expect_length(res@data, 500)
  expect_length(res@info, 44)
  expect_equal(res@originator, ".Risoe.BINfileData2RLum.Data.Curve")

  res1 <- .Risoe.BINfileData2RLum.Data.Curve(CWOSL.SAR.Data,
                                             pos = 1, set = 2, run = 1)
  expect_s4_class(res1, "RLum.Data.Curve")
  expect_equal(res1@data, res@data)
  expect_equal(res1@info, res@info)
  expect_equal(res1@originator, res@originator)
})

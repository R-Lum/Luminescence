context("get_RLum")

data(ExampleData.DeValues, envir = environment())
temp <- calc_CentralDose(ExampleData.DeValues$CA1, plot = FALSE, verbose = FALSE)

temp_RLumDataCurve <- set_RLum(class = "RLum.Data.Curve")
temp_RLumDataImage <- set_RLum(class = "RLum.Data.Image")
temp_RLumDataSpectrum <- set_RLum(class = "RLum.Data.Spectrum")
temp_RLumAnalysis <- set_RLum(class = "RLum.Analysis")
temp_RLumResults <- set_RLum(class = "RLum.Results")



test_that("check class and length of output", {
  testthat::skip_on_cran()

  expect_is(get_RLum(temp), class = "data.frame")
  expect_is(get_RLum(temp, data.object = "args"), class = "list")

  ##test objects
  expect_is(get_RLum(temp_RLumDataCurve), class = "matrix")
  expect_is(get_RLum(temp_RLumDataImage), class = "RasterBrick")
  expect_is(get_RLum(temp_RLumDataSpectrum), class = "matrix")
  expect_null(get_RLum(temp_RLumAnalysis))
  expect_null(get_RLum(temp_RLumResults))

})

test_that("check get_RLum on a list and NULL", {
  testthat::skip_on_cran()

  object <- set_RLum(class = "RLum.Analysis", records = rep(set_RLum(class = "RLum.Data.Curve"), 10))
  expect_warning(get_RLum(object, recordType = "test"))

  expect_is(get_RLum(NULL), "NULL")

  ##check class argument
  a <- list(set_RLum("RLum.Results"), set_RLum("RLum.Analysis", records = list(set_RLum("RLum.Data.Curve"))))
  expect_is(get_RLum(a, class = "test", drop = FALSE), class = "list")
  expect_is(get_RLum(a, class = "RLum.Results", drop = FALSE), class = "list")
})

data(ExampleData.DeValues, envir = environment())
temp <- calc_CentralDose(ExampleData.DeValues$CA1, plot = FALSE, verbose = FALSE)

temp_RLumDataCurve <- set_RLum(class = "RLum.Data.Curve")
temp_RLumDataImage <- set_RLum(class = "RLum.Data.Image")
temp_RLumDataSpectrum <- set_RLum(class = "RLum.Data.Spectrum")
temp_RLumAnalysis <- set_RLum(class = "RLum.Analysis")
temp_RLumResults <- set_RLum(class = "RLum.Results")

test_that("check class and length of output", {
  testthat::skip_on_cran()

  expect_s3_class(get_RLum(temp), class = "data.frame")
  expect_type(get_RLum(temp, data.object = "args"), "list")

  ##test objects
  expect_type(get_RLum(temp_RLumDataCurve), "double")
  expect_type(get_RLum(temp_RLumDataImage), "logical")
  expect_type(get_RLum(temp_RLumDataSpectrum), "logical")
  expect_null(suppressWarnings(get_RLum(temp_RLumAnalysis)))
  expect_null(get_RLum(temp_RLumResults))
})

test_that("check get_RLum on a list and NULL", {
  testthat::skip_on_cran()

  object <- set_RLum(class = "RLum.Analysis", records = rep(set_RLum(class = "RLum.Data.Curve"), 10))
  expect_warning(get_RLum(object, recordType = "test"),
                 "This request produced an empty list of records")
  expect_null(get_RLum(NULL), "NULL")

  expect_warning(res <- get_RLum(list(temp, "test")),
                 "object #2 in the list was not of type 'RLum'")
  expect_length(res, 2)
  res <- get_RLum(list(temp, temp_RLumAnalysis), null.rm = TRUE)
  expect_length(res, 1)

  ##check list of such objects
  l <- list(object, object)
  l[[1]]@records[[1]]@recordType <- "IRSL"
  expect_length(suppressWarnings(get_RLum(l, recordType = "IRSL", null.rm = FALSE)), 2)
  expect_length(suppressWarnings(get_RLum(l, recordType = "IRSL", null.rm = TRUE)), 0)

  ##check class argument
  a <- list(set_RLum("RLum.Results"), set_RLum("RLum.Analysis", records = list(set_RLum("RLum.Data.Curve"))))
  expect_type(get_RLum(a, class = "test", drop = FALSE), "list")
  expect_type(get_RLum(a, class = "test", drop = TRUE), "list")
  expect_type(get_RLum(a, class = "RLum.Results", drop = FALSE), "list")
  expect_type(get_RLum(a, class = "RLum.Analysis", drop = TRUE), "list")
  expect_type(get_RLum(list(temp_RLumResults, temp_RLumAnalysis)), "list")
})

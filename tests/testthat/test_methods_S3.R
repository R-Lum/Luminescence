test_that("Test various S3 methods", {
  testthat::skip_on_cran()

  ## RLum.Analysis
  data(ExampleData.RLum.Analysis, envir = environment())
  analysis <- IRSAR.RF.Data

  expect_silent(plot(analysis))
  expect_silent(hist(analysis))
  expect_type(summary(analysis), "list")
  expect_s4_class(subset(analysis), "RLum.Analysis")
  expect_equal(length(analysis), 2)
  expect_length(rep(analysis, 2), 2)
  expect_equal(names(analysis), c("RF", "RF"))
  expect_type(as.list(analysis), "list")
  expect_equal(is(analysis), c("RLum.Analysis", "RLum"))
  expect_s4_class(merge(analysis, analysis), "RLum.Analysis")
  expect_length(unlist(analysis, recursive = TRUE), 4)
  expect_length(unlist(analysis, recursive = FALSE), 2)
  expect_length(analysis[1], 1)
  expect_length(analysis["RF"], 2)
  expect_s4_class(analysis[[1]], "RLum.Data.Curve")
  expect_type(analysis[["RF"]], "list")
  expect_length(analysis$RF, 2)
  expect_true(is.RLum(analysis))
  expect_true(is.RLum.Analysis(analysis))
  expect_false(is.RLum.Data(analysis))

  ## RLum.Results
  result <- calc_SourceDoseRate(
    measurement.date = "2012-01-27",
    calib.date = "2014-12-19",
    calib.dose.rate = 0.0438,
    calib.error = 0.0019)

  expect_silent(plot(result))
  expect_silent(plot(list(result, result)))
  expect_silent(hist(result))
  expect_s3_class(summary(result), "data.frame")
  expect_equal(length(result), 3)
  expect_length(rep(result, 2), 2)
  expect_equal(names(result), c("dose.rate", "parameters", "call"))
  expect_type(as.list(result), "list")
  expect_equal(is(result), c("RLum.Results", "RLum"))
  expect_s4_class(merge(result, result), "RLum.Results")
  expect_visible(result[1])
  expect_visible(result[[1]])
  expect_visible(result$dose.rate)
  expect_true(is.RLum(result))
  expect_true(is.RLum.Results(result))
  expect_false(is.RLum.Data(result))

  ## RLum.Data.Curve
  data(ExampleData.CW_OSL_Curve, envir = environment())
  curve <- set_RLum(
      class = "RLum.Data.Curve",
      data = as.matrix(ExampleData.CW_OSL_Curve),
      curveType = "measured",
      recordType = "OSL",
      info = list(a = "test")
  )

  expect_silent(plot(curve))
  expect_silent(plot(list(curve, curve)))
  expect_silent(hist(curve))
  expect_s3_class(summary(curve), "table")
  expect_equal(length(curve), 40)
  expect_equal(dim(curve), c(1000, 2))
  expect_s4_class(bin(curve), "RLum.Data.Curve")
  expect_length(rep(curve, 2), 2)
  expect_equal(names(curve), "a")
  expect_s3_class(as.data.frame(curve), "data.frame")
  expect_type(as.list(curve), "list")
  expect_vector(as.matrix(curve))
  expect_equal(is(curve), c("RLum.Data.Curve", "RLum.Data", "RLum"))
  expect_s4_class(merge(curve, curve), "RLum.Data.Curve")
  expect_visible(curve + curve)
  expect_visible(curve - curve)
  expect_visible(curve * curve)
  expect_visible(curve / curve)
  expect_vector(curve[1])
  expect_equal(curve$a, c(a = "test"))
  expect_true(is.RLum(curve))
  expect_true(is.RLum.Data(curve))
  expect_true(is.RLum.Data.Curve(curve))

  ## RLum.Data.Image
  data(ExampleData.RLum.Data.Image, envir = environment())
  image <- ExampleData.RLum.Data.Image
  image3 <- set_RLum("RLum.Data.Image",
                     data = array(runif(300, 0, 255), c(10, 10, 3)),
                     info = list(NumFrames = 3))

  expect_silent(plot(image))
  expect_silent(plot(list(image, image)))
  expect_silent(plot(image3))
  expect_silent(hist(image))
  expect_silent(hist(image3))
  expect_s3_class(summary(image), "table")
  expect_s3_class(summary(image3), "table")
  expect_length(rep(image, 2), 2)
  expect_equal(names(image)[1:3],
               c("ControllerVersion", "LogicOutput", "AmpHiCapLowNoise"))
  expect_equal(names(image3), "NumFrames")
  expect_type(as.list(image), "list")
  expect_length(as.list(image), 1)
  expect_length(as.list(image3), 3)
  expect_vector(as.matrix(image))
  expect_error(as.matrix(image3),
               "No viable coercion to matrix, object contains multiple frames")
  expect_equal(is(image), c("RLum.Data.Image", "RLum.Data", "RLum"))
  expect_error(merge(image, image),
               "Merging of 'RLum.Data.Image' objects is currently not supported")
  expect_vector(image[1])
  expect_error(image3[1],
               "No viable coercion to matrix, object contains multiple frames")
  expect_true(is.RLum(image))
  expect_true(is.RLum.Data(image))
  expect_true(is.RLum.Data.Image(image))

  ## RLum.Data.Spectrum
  data(ExampleData.XSYG, envir = environment())
  spectrum <- TL.Spectrum

  expect_silent(plot(spectrum))
  expect_silent(plot(list(spectrum, spectrum)))
  expect_s4_class(bin(spectrum), "RLum.Data.Spectrum")
  expect_equal(dim(spectrum), c(1024, 24))
  expect_length(rep(spectrum, 2), 2)
  expect_equal(names(spectrum)[1:3], c("state", "parentID", "startDate"))
  expect_type(row.names(spectrum), "character")
  expect_s3_class(as.data.frame(spectrum), "data.frame")
  expect_type(as.list(spectrum), "list")
  expect_length(as.list(spectrum), 24)
  expect_vector(as.matrix(spectrum))
  expect_equal(is(spectrum), c("RLum.Data.Spectrum", "RLum.Data", "RLum"))
  expect_s4_class(merge(spectrum, spectrum), "RLum.Data.Spectrum")
  expect_visible(spectrum + spectrum)
  expect_visible(spectrum - spectrum)
  expect_visible(spectrum * spectrum)
  expect_visible(spectrum / spectrum)
  expect_vector(spectrum[1])
  expect_true(is.RLum(spectrum))
  expect_true(is.RLum.Data(spectrum))
  expect_true(is.RLum.Data.Spectrum(spectrum))

  ## Risoe.BINfileData
  data(ExampleData.BINfileData, envir = environment())
  risoe <- CWOSL.SAR.Data

  expect_silent(plot(risoe))
  expect_error(plot(list(risoe, risoe)))
  expect_error(subset(risoe, ERROR == 1))
  expect_warning(subset(risoe, ID == 1, error = TRUE),
                 "Argument not supported and skipped")
  expect_length(subset(risoe, ID == 1), 1)
  expect_length(subset(risoe, ID == 1, records.rm = FALSE), 720)
  expect_equal(length(risoe), 720)
  expect_equal(names(risoe)[1:40], c(rep("TL", 24), rep("OSL", 16)))
  expect_s3_class(as.data.frame(risoe), "data.frame")
})

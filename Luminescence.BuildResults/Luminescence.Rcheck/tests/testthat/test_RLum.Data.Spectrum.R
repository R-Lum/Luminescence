test_that("check class", {
  testthat::skip_on_cran()
  local_edition(3)

  ##set empty spectrum object and show it
  expect_output(show(set_RLum(class = "RLum.Data.Spectrum")))

  ##check replacements
  object <- set_RLum(class = "RLum.Data.Spectrum")
  expect_s4_class(set_RLum(class = "RLum.Data.Spectrum", data = object), class = "RLum.Data.Spectrum")

  ##check get_RLum
  object <- set_RLum(class = "RLum.Data.Spectrum", data = object, info = list(a = "test"))
  expect_error(get_RLum(object, info.object = "est"), regexp = "Invalid element name. Valid names are: a")
  expect_error(get_RLum(object, info.object = 1L), "'info.object' has to be a character!")
  expect_type(get_RLum(object, info.object = "a"), "character")

  ##test method names
  expect_type(names(object), "character")

  ##test bin_RLum()
  expect_error(bin_RLum.Data(object, bin_size.col = "test"),
               "'bin_size.row' and 'bin_size.col' must be of class 'numeric'!")
  object@data <- matrix(data = rep(1:20, each = 10), ncol = 20)
  rownames(object@data) <- 1:10
  colnames(object@data) <- 1:20
  expect_s4_class(object = bin_RLum.Data(object, bin_size.row = 2), "RLum.Data.Spectrum")

  expect_s4_class(object = bin_RLum.Data(object, bin_size.row = 1, bin_size.col = 2), "RLum.Data.Spectrum")

  ##check conversions
  expect_s4_class(as(object = data.frame(x = 1:10), Class = "RLum.Data.Spectrum"), "RLum.Data.Spectrum")
  expect_s3_class(as(set_RLum("RLum.Data.Spectrum"), "data.frame"), "data.frame")
  expect_s4_class(as(object = matrix(1:10,ncol = 2), Class = "RLum.Data.Spectrum"), "RLum.Data.Spectrum")

})

test_that("check class", {
  testthat::skip_on_cran()

  ##set empty spectrum object and show it
  expect_output(show(set_RLum(class = "RLum.Data.Spectrum")))

  ##check replacements
  object <- set_RLum(class = "RLum.Data.Spectrum")
  expect_s4_class(set_RLum(class = "RLum.Data.Spectrum", data = object), class = "RLum.Data.Spectrum")

  ##check get_RLum
  object <- set_RLum(class = "RLum.Data.Spectrum", data = object, info = list(a = "test"))
  expect_error(get_RLum(object, info.object = "test"),
               "Invalid element name, valid names are: 'a'")
  expect_error(get_RLum(object, info.object = 1L),
               "'info.object' should be of class 'character'")
  expect_error(get_RLum(object, info.object = list()),
               "'info.object' should be of class 'character' or NULL and have length 1")
  expect_type(get_RLum(object, info.object = "a"), "character")

  ##test method names
  expect_type(names(object), "character")

  ##check conversions
  expect_s4_class(as(object = data.frame(x = 1:10), Class = "RLum.Data.Spectrum"), "RLum.Data.Spectrum")
  expect_s3_class(as(set_RLum("RLum.Data.Spectrum"), "data.frame"), "data.frame")
  expect_s4_class(as(object = matrix(1:10,ncol = 2), Class = "RLum.Data.Spectrum"), "RLum.Data.Spectrum")
  expect_s4_class(as(list(1:10), "RLum.Data.Spectrum"),
                  "RLum.Data.Spectrum")
  expect_s4_class(as(list(), "RLum.Data.Spectrum"),
                  "RLum.Data.Spectrum")
  expect_type(as(set_RLum("RLum.Data.Spectrum"), "list"),
              "list")
})

test_that("Test general functionality", {
  testthat::skip_on_cran()
  local_edition(3)

  ## load example data
  data(ExampleData.RLum.Data.Image, envir = environment())
  data(ExampleData.XSYG, envir = environment())

  ##crash function
  expect_error(write_R2TIFF(object = "test"),
        "\\[write\\_R2TIFF\\(\\)\\] Only RLum.Data.Image and RLum.Data.Spectrum objects are supported!")

  expect_error(write_R2TIFF(object = ExampleData.RLum.Data.Image, file = "error/error"),
               "\\[write\\_R2TIFF\\(\\)\\] Path does not exist!")

  ## export RLum.Data.Image
  expect_null(write_R2TIFF(ExampleData.RLum.Data.Image, file = tempfile(fileext = "tiff")))

  ## export RLum.Data.Spectrum
  expect_null(write_R2TIFF(TL.Spectrum, file = tempfile(fileext = "tiff")))

  ## a list
  expect_null(write_R2TIFF(list(ExampleData.RLum.Data.Image, TL.Spectrum), file = tempfile(fileext = "tiff")))

})

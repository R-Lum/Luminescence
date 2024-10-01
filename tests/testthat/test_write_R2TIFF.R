test_that("Test general functionality", {
  testthat::skip_on_cran()

  ## load example data
  data(ExampleData.RLum.Data.Image, envir = environment())
  data(ExampleData.XSYG, envir = environment())

  ##crash function
  expect_error(write_R2TIFF(object = "test"),
               "[write_R2TIFF()] 'object' should be of class 'RLum.Data.Image'",
               fixed = TRUE)

  expect_error(write_R2TIFF(ExampleData.RLum.Data.Image, file = "error/error"),
               "[write_R2TIFF()] Path does not exist",
               fixed = TRUE)

  ## export RLum.Data.Image
  expect_null(write_R2TIFF(ExampleData.RLum.Data.Image, file = tempfile(fileext = "tiff")))

  ## export RLum.Data.Spectrum
  expect_null(write_R2TIFF(TL.Spectrum, file = tempfile(fileext = "tiff")))

  ## a list
  expect_null(write_R2TIFF(list(ExampleData.RLum.Data.Image, TL.Spectrum), file = tempfile(fileext = "tiff")))
})

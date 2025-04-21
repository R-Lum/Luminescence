## load data
data(ExampleData.RLum.Data.Image, envir = environment())
data(ExampleData.XSYG, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(write_R2TIFF(object = "test"),
               "[write_R2TIFF()] 'object' should be of class 'RLum.Data.Image'",
               fixed = TRUE)
  expect_error(write_R2TIFF(ExampleData.RLum.Data.Image, file = "error/error"),
               "[write_R2TIFF()] Path does not exist",
               fixed = TRUE)
  expect_error(write_R2TIFF(set_RLum("RLum.Data.Image")),
               "Empty RLum.Data.Image object detected")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## export RLum.Data.Image
  expect_null(write_R2TIFF(ExampleData.RLum.Data.Image, file = tempfile(fileext = "tiff")))

  ## export image stack
  t_stack <- set_RLum(
    "RLum.Data.Image",
    data = array(c(ExampleData.RLum.Data.Image@data, ExampleData.RLum.Data.Image@data),
                 dim = c(1340, 101, 2)))
  expect_null(write_R2TIFF(t_stack, file = tempfile(fileext = "tiff")))

  ## export RLum.Data.Spectrum
  expect_null(write_R2TIFF(TL.Spectrum, file = tempfile(fileext = "tiff")))

  ## a list
  expect_null(write_R2TIFF(list(ExampleData.RLum.Data.Image, TL.Spectrum), file = tempfile(fileext = "tiff")))
  expect_null(write_R2TIFF(list()))
})

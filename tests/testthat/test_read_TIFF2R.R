test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(read_TIFF2R(data.frame()),
               "'file' should be of class 'character'")
  expect_error(read_TIFF2R(character(0)),
               "'file' should have length 1")
  expect_error(object = read_TIFF2R(file = "text"),
               "[read_TIFF2R()] File does not exist or is not readable",
               fixed = TRUE)
})

test_that("check functionality", {
  testthat::skip_on_cran()

  file <- system.file("extdata", "TIFFfile.tif", package = "Luminescence")
  SW({
  expect_s4_class(read_TIFF2R(file), "RLum.Data.Image")

  ## check image stack
  t <- expect_s4_class(read_TIFF2R(list(file, file), merge2stack = TRUE), "RLum.Data.Image")
  expect_equal(dim(t@data)[3], 2)

  t <- expect_type(read_TIFF2R(list(file, file), merge2stack = FALSE), "list")
  expect_length(t, 2)

  t <- expect_type(read_TIFF2R(c(file, file), merge2stack = FALSE), "list")
  expect_length(t, 2)

  })
})

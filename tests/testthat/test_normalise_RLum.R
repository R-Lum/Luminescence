data(ExampleData.CW_OSL_Curve, envir = environment())
## curve
temp <-
  set_RLum(
      class = "RLum.Data.Curve",
      recordType = "OSL",
      data = as.matrix(ExampleData.CW_OSL_Curve)
  )

##create RLum.Analysis object
temp_analysis <- set_RLum("RLum.Analysis", records = list(temp, temp))

## spectra object
spectra <- set_RLum("RLum.Data.Spectrum", data = matrix(1:10, ncol = 2))

## array
image <- set_RLum("RLum.Data.Image", data = array(1:12, dim = c(2,3,2)))

test_that("check class and length of output", {
  testthat::skip_on_cran()

  ## break function
  expect_error(normalise_RLum(temp, norm = "error"), "'norm' should be one of 'min', 'max', 'first', 'last' or 'huot'")
  expect_error(normalise_RLum(temp, norm = c(1,1)), "'norm' should be of class 'logical', 'character' or 'numeric' and have length 1")


  ##standard tests
  expect_s4_class(temp, class = "RLum.Data.Curve")
  expect_snapshot_RLum(normalise_RLum(temp))

  ## spectrum
  expect_s4_class(spectra, class = "RLum.Data.Spectrum")
  expect_snapshot_RLum(normalise_RLum(spectra))

  ## test also an array, which indeed works differently
  expect_s4_class(image, class = "RLum.Data.Image")
  t <- expect_snapshot_RLum(normalise_RLum(image, global = TRUE))
  expect_snapshot_RLum(normalise_RLum(image, global = FALSE))
  expect_s4_class(normalise_RLum(image, global = TRUE), "RLum.Data.Image")
  expect_s4_class(normalise_RLum(image, norm = "first", global = TRUE), "RLum.Data.Image")
  expect_s4_class(normalise_RLum(image, norm = "last", global = TRUE), "RLum.Data.Image")
  expect_s4_class(normalise_RLum(image, norm = "first", global = FALSE), "RLum.Data.Image")
  expect_s4_class(normalise_RLum(image, norm = "last", global = FALSE), "RLum.Data.Image")

  ## tests as for the parameters are already covered in the for the
  ## internal function .normalise_curve() and should not be repeated
  ## here

  ##test on a list
  ##RLum list
  ## standard normalisation
  t <- expect_type(normalise_RLum(list(temp, temp)), "list")
  expect_equal(as.numeric(t[[1]]@data[1,2]), 1)

  ## list with non-RLum object
  t <- expect_type(normalise_RLum(list(temp, iris)), "list")
  expect_equal(as.numeric(t[[1]]@data[1,2]), 1)
  expect_equal(t[[2]], iris)

  ## normalise to "last"
  t <- expect_type(normalise_RLum(list(temp, temp), norm = "last"), "list")
  expect_equal(as.numeric(t[[1]]@data[1,2]), 317.5, tolerance = 0.01)

  ##test on an RLum.Analysis-object and a list of such objects
  t <- expect_s4_class(normalise_RLum(temp_analysis), "RLum.Analysis")
  expect_equal(as.numeric(t@records[[2]]@data[1,2]), 1)

  ## check the modification for an argument
  t <- expect_s4_class(normalise_RLum(temp_analysis, norm = "last"), "RLum.Analysis")
  expect_equal(as.numeric(t@records[[2]]@data[1,2]), 317.5, tolerance = 0.01)

  ## check the list of such objects
  t <- expect_type(normalise_RLum(list(temp_analysis, temp_analysis)), "list")
  expect_equal(as.numeric(t[[1]]@records[[2]]@data[1,2]), 1)

  ## check Image objects within
  t <- expect_s4_class(normalise_RLum(set_RLum("RLum.Analysis", records = list(image))), "RLum.Analysis")
  expect_equal(t@records[[1]]@data[2,3,2], 1)
  t <- expect_s4_class(normalise_RLum(set_RLum("RLum.Analysis", records = list(image)), global = FALSE), "RLum.Analysis")
  expect_equal(t@records[[1]]@data[2,3,1], t@records[[1]]@data[2,3,2])
  t <- expect_type(
    normalise_RLum(
      list(
        set_RLum("RLum.Analysis", records = list(image)),
        set_RLum("RLum.Analysis", records = list(image))), global = FALSE), "list")
  expect_equal(t[[1]]@records[[1]]@data[2,3,2], t[[1]]@records[[1]]@data[2,3,1])
  t <- expect_type(
    normalise_RLum(
      list(
        set_RLum("RLum.Analysis", records = list(image)),
        set_RLum("RLum.Analysis", records = list(image))), global = TRUE), "list")
  expect_equal(t[[1]]@records[[1]]@data[2,3,1], 0.5, tolerance = 0.001)

})

test_that("snapshot tests", {
 testthat::skip_on_cran()

 small <-set_RLum(class = "RLum.Data.Curve", recordType = "OSL",
                  data = as.matrix(ExampleData.CW_OSL_Curve[1:150, ]))
 expect_snapshot_RLum(normalise_RLum(small, norm = TRUE))
 expect_snapshot_RLum(normalise_RLum(small, norm = "min"))
 expect_snapshot_RLum(normalise_RLum(small, norm = "max"))
 expect_snapshot_RLum(normalise_RLum(small, norm = "first"))
 expect_snapshot_RLum(normalise_RLum(small, norm = "last"))
 expect_snapshot_RLum(normalise_RLum(small, norm = 2.2))
 expect_snapshot_RLum(normalise_RLum(small, norm = "huot"))
})

test_that("check class", {
  testthat::skip_on_cran()

  ##set empty curve object and show it
  expect_output(show(set_RLum(class = "RLum.Data.Curve")))

  ##check replacements fo
  object <- set_RLum(class = "RLum.Data.Curve")
  expect_s4_class(set_RLum(class = "RLum.Data.Curve", data = object), class = "RLum.Data.Curve")

  ##check get_RLum
  object <- set_RLum(class = "RLum.Data.Curve", data = object, info = list(a = "test"))
  expect_warning(get_RLum(object, info.object = "error"),
                 "Invalid 'info.object' name, valid names are:")

  ##test names
  expect_type(names(object), "character")

  ##test bin
  expect_warning(bin_RLum.Data(object, bin_size = -2), "Argument 'bin_size' invalid, nothing was done!")

  ##check conversions
  expect_s4_class(as(object = list(1:10), Class = "RLum.Data.Curve"), "RLum.Data.Curve")
  expect_s4_class(as(list(), "RLum.Data.Curve"),
                  "RLum.Data.Curve")
  expect_type(as(object = object, Class = "list"), "list")
  expect_s4_class(as(object = matrix(1:10,ncol = 2), Class = "RLum.Data.Curve"), "RLum.Data.Curve")

  ## test melt simple
  expect_type(melt_RLum(object = object), "list")

  ## test melt more complicated
  data(ExampleData.XSYG, envir = environment())
  t <- melt_RLum(OSL.SARMeasurement[[2]][[1]])
  expect_length(t, n = 4)

  ## provide a list
  expect_length(melt_RLum(OSL.SARMeasurement[[2]]@records[1:5]), n = 4)

  ## test problematic records
  l <- c(OSL.SARMeasurement[[2]]@records[1:5], "non-RLum")
  expect_length(melt_RLum(l), n = 4)

  l <- list("non-RLum")
  expect_null(melt_RLum(l))

  l <- c(list("non-RLum"), set_RLum("RLum.Results"))
  expect_type(melt_RLum(l), "list")
})

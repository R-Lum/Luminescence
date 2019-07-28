context("RLum.Data.Image")

test_that("check class ", {
  testthat::skip_on_cran()

  ##load example data
  data(ExampleData.RLum.Data.Image, envir = environment())

  ##set-method
  ##set empty class
  expect_s4_class(set_RLum(class = "RLum.Data.Image"), "RLum.Data.Image")

  ##overwrite only data
  expect_s4_class(set_RLum(class = "RLum.Data.Image", data = set_RLum("RLum.Data.Image")), "RLum.Data.Image")

  ##show-method
  ##show example data
  expect_output(show(ExampleData.RLum.Data.Image))

  ##get-method
  expect_error(get_RLum(ExampleData.RLum.Data.Image, info.object = 1), regexp = "'info.object' has to be a character!")
  expect_error(get_RLum(ExampleData.RLum.Data.Image, info.object = "unknown"))
  expect_is(get_RLum(ExampleData.RLum.Data.Image, info.object = "NumFrames"), "integer")

  ##names
  expect_is(names_RLum(ExampleData.RLum.Data.Image), class = "character")

  ##conversions
  ##from matrix and to matrix
  expect_is(as(matrix(1:10, ncol = 2), "RLum.Data.Image"), "RLum.Data.Image")
  expect_is(as(ExampleData.RLum.Data.Image, "matrix"), "matrix")

  ##from data.frame and to data.frame
  df <- as.data.frame(as(ExampleData.RLum.Data.Image, "matrix"))
  expect_is(as(df, "RLum.Data.Image"), "RLum.Data.Image")
  expect_is(as(ExampleData.RLum.Data.Image, "data.frame"), "data.frame")

})

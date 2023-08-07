test_that("check class ", {
  testthat::skip_on_cran()
  local_edition(3)

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
  expect_type(get_RLum(ExampleData.RLum.Data.Image, info.object = "NumFrames"), "integer")

  ##names
  expect_type(names_RLum(ExampleData.RLum.Data.Image), "character")

  ##conversions
  ##from matrix and to matrix
  expect_s4_class(as(matrix(1:10, ncol = 2), "RLum.Data.Image"), "RLum.Data.Image")
  expect_type(as(ExampleData.RLum.Data.Image, "matrix"), "integer")

  ##from data.frame and to data.frame
  df <- as.data.frame(as(ExampleData.RLum.Data.Image, "matrix"))
  expect_s4_class(as(df, "RLum.Data.Image"), "RLum.Data.Image")
  expect_s3_class(as(ExampleData.RLum.Data.Image, "data.frame"), "data.frame")

  ## to and from array
  expect_type(as(ExampleData.RLum.Data.Image, "array"), "integer")
  from_array <- expect_s4_class(as(array(1,dim = c(10,10,2)), "RLum.Data.Image"), "RLum.Data.Image")

  ## to and from list
  expect_s4_class(as(list(matrix(1, nrow = 10, ncol = 5), matrix(1, nrow = 10, ncol = 5)), "RLum.Data.Image"),
                  "RLum.Data.Image")
  expect_type(as(ExampleData.RLum.Data.Image, "list"), "list")

  ## check edge cases
  expect_error(as(from_array, "matrix"), "No viable coercion to matrix, object contains multiple frames. Please convert to array instead.")
  expect_error(as(from_array, "data.frame"), "No viable coercion to data.frame, object contains multiple frames.")

})

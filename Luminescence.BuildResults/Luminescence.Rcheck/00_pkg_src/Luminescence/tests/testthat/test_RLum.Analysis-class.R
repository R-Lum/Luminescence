context("RLum.Analysis-class")

test_that("Check the example and the numerical values", {
  testthat::skip_on_cran()
  ##construct empty object
  tmp <- set_RLum("RLum.Analysis", protocol = "testthat", 
                  records = lapply(1:20, function(x)  { 
                    set_RLum("RLum.Data.Curve", recordType = "test", data = matrix(1:10, ncol = 2), 
                             info = list(el = as.character(x))) 
                  }), 
                  info = list(el = "test"))
  data("ExampleData.RLum.Analysis")
  obj <- IRSAR.RF.Data
  
  ## as()
  expect_is(as(tmp, "list"), "list")
  expect_is(as(list(), "RLum.Analysis"), "RLum.Analysis")
  
  ## output
  expect_output(print(as(list(), "RLum.Analysis")), regexp = "This is an empty object")
  expect_is(set_RLum("RLum.Analysis", protocol = "testthat", 
                     records = set_RLum("RLum.Analysis", protocol = "nest", records = list(matrix(1:10, ncol = 2))), 
                     info = list(el = "test")), "RLum.Analysis")
  
  ## get_RLum
  expect_length(get_RLum(obj, subset = (recordType == "RF")), 2)
  expect_null(get_RLum(obj, subset = (recordType == "")))
  expect_length(get_RLum(tmp, subset = (el == "2")), 1)
  expect_is(get_RLum(tmp, subset = (el == "2")), "RLum.Analysis")
  expect_is(get_RLum(tmp, info.object = "el"), "character")
  expect_warning(get_RLum(tmp, info.object = "missing"), regexp = "Invalid info.object name")
  
})


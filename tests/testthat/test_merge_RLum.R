## load data
data(ExampleData.RLum.Analysis, envir = environment())
data(ExampleData.RLum.Data.Image, envir = environment())
o1 <- IRSAR.RF.Data
o2 <- IRSAR.RF.Data
c1 <- as(object = list(1:10), Class = "RLum.Data.Curve")
s1 <- as(object = matrix(1:20, 4, 5, dimnames = list(1:4, 1:5)),
         Class = "RLum.Data.Spectrum")
r1 <- as(object = list(1:10), Class = "RLum.Results")

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(merge_RLum("error"),
               "[merge_RLum()] 'objects' should be of class 'list'",
               fixed = TRUE)
  expect_error(merge_RLum(list(o1, o2, "test")),
               "All elements of 'objects' should be of class 'RLum'")
  expect_error(merge_RLum(list(r1, c1)),
               "Only similar input objects in the list are supported")
  expect_error(merge_RLum(list(ExampleData.RLum.Data.Image)),
               "Merging of 'RLum.Data.Image' objects is currently not supported")

  expect_warning(merge_RLum(list(NULL)),
                 "Nothing was merged as the object list was found to be empty")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## simple test
  expect_s4_class(merge_RLum(list(o1,o2)), "RLum.Analysis")
  expect_s4_class(merge_RLum(list(c1,c1)), "RLum.Data.Curve")
  expect_s4_class(merge_RLum(list(s1,s1)), "RLum.Data.Spectrum")
  expect_s4_class(merge_RLum(list(r1,r1)), "RLum.Results")

  ## with null objects
  expect_s4_class(merge_RLum(list(o1,o2, NULL)), "RLum.Analysis")

  ## single object
  expect_s4_class(
    merge_RLum(list(o1)), "RLum.Analysis")
})

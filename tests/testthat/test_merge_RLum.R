test_that("Merge tests", {
  testthat::skip_on_cran()

  ##load data
  data(ExampleData.RLum.Analysis, envir = environment())

  ## set objects
  o1 <- IRSAR.RF.Data
  o2 <- IRSAR.RF.Data
  c1 <- as(object = list(1:10), Class = "RLum.Data.Curve")
  s1 <- as(object = matrix(1:20, 4, 5, dimnames = list(1:4, 1:5)),
           Class = "RLum.Data.Spectrum")
  r1 <- as(object = list(1:10), Class = "RLum.Results")

  ## simple test
  expect_s4_class(merge_RLum(list(o1,o2)), "RLum.Analysis")
  expect_s4_class(merge_RLum(list(c1,c1)), "RLum.Data.Curve")
  expect_s4_class(merge_RLum(list(s1,s1)), "RLum.Data.Spectrum")
  expect_s4_class(merge_RLum(list(r1,r1)), "RLum.Results")

  ## with null objects
  expect_s4_class(merge_RLum(list(o1,o2, NULL)), "RLum.Analysis")

  ## with unwanted objects
  expect_error(merge_RLum(list(o1,o2, "test")),
               "[merge_RLum()] All elements of 'objects' should be of class 'RLum'",
               fixed = TRUE)

  ## single object
  expect_s4_class(
    merge_RLum(list(o1)), "RLum.Analysis")

  ## zero objects produces warnings
  expect_warning(
    merge_RLum(list(NULL)),
    regexp = "\\[merge\\_RLum\\(\\)\\] Nothing was merged as the .+")

  ## crash with non-list
  expect_error(merge_RLum("errr"),
               "[merge_RLum()] 'objects' should be of class 'list'",
               fixed = TRUE)

  ## mixed objects
  expect_error(merge_RLum(list(r1, c1)),
               "Only similar input objects in the list are supported")

  ## unsupported
  data(ExampleData.RLum.Data.Image, envir = environment())
  expect_error(merge_RLum(list(ExampleData.RLum.Data.Image)),
               "Merging of 'RLum.Data.Image' objects is currently not supported")
})

test_that("input validation", {
  testthat::skip_on_cran()

  data(ExampleData.RLum.Analysis, envir = environment())
  o1 <- IRSAR.RF.Data
  c1 <- as(object = list(1:10), Class = "RLum.Data.Curve")
  r1 <- as(object = list(1:10), Class = "RLum.Results")

  expect_error(merge_RLum.Analysis(),
               "'objects' should be of class 'list'")
  expect_error(merge_RLum.Analysis(o1),
               "'objects' should be of class 'list'")
  expect_error(merge_RLum.Analysis(list()),
               "'objects' cannot be an empty list")
  expect_error(merge_RLum.Analysis(list(c1)),
               "At least one input object in the list has to be of class")
  expect_error(merge_RLum.Analysis(list(o1, "test")),
               "All elements of 'object' should be of class 'RLum'")
  expect_error(merge_RLum.Analysis(list(o1, r1)),
               "'objects' should be of class 'RLum.Analysis' or 'RLum.Data'")

  expect_s4_class(merge_RLum.Analysis(list(o1)),
                  "RLum.Analysis")
  expect_s4_class(merge_RLum.Analysis(list(c1, o1)),
                  "RLum.Analysis")
})

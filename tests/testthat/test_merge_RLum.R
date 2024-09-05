test_that("Merge tests", {
  testthat::skip_on_cran()

  ##load data
  data(ExampleData.RLum.Analysis, envir = environment())

  ## set objects
  o1 <- IRSAR.RF.Data
  o2 <- IRSAR.RF.Data
  c1 <- as(object = list(1:10), Class = "RLum.Data.Curve")
  r1 <- as(object = list(1:10), Class = "RLum.Results")

  ## simple test
  expect_s4_class(merge_RLum(list(o1,o2)), "RLum.Analysis")
  expect_s4_class(merge_RLum(list(c1,c1)), "RLum.Data.Curve")
  expect_s4_class(merge_RLum(list(r1,r1)), "RLum.Results")

  ## with null objects
  expect_s4_class(merge_RLum(list(o1,o2, NULL)), "RLum.Analysis")

  ## with unwanted objects
  expect_error(merge_RLum(list(o1,o2, "test")),
               regexp = "\\[merge\\_RLum\\(\\)\\]: At least element \\#3 is not of class 'RLum' or a derivative class\\!")

  ## single object
  expect_s4_class(
    merge_RLum(list(o1)), "RLum.Analysis")

  ## zero objects produces warnings
  expect_warning(
    merge_RLum(list(NULL)),
    regexp = "\\[merge\\_RLum\\(\\)\\] Nothing was merged as the .+")

  ## crash with non-list
  expect_error(merge_RLum("errr"), "\\[merge\\_RLum\\(\\)\\] argument 'objects' .*")

  ## mixed objects
  expect_error(merge_RLum(list(r1, c1)),
               "So far only similar input objects in the list are supported")

  ## unsupported
  data(ExampleData.RLum.Data.Image, envir = environment())
  expect_error(merge_RLum(list(ExampleData.RLum.Data.Image)),
               "Merging of 'RLum.Data.Image' objects is currently not supported")
  data(ExampleData.XSYG, envir = environment())
  expect_error(merge_RLum(list(TL.Spectrum)),
               "Merging of 'RLum.Data.Spectrum' objects is currently not supported")
})

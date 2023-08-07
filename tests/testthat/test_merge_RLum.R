test_that("Merge tests", {
  testthat::skip_on_cran()
  local_edition(3)

  ##load data
  data(ExampleData.RLum.Analysis, envir = environment())

  ## set objects
  o1 <- IRSAR.RF.Data
  o2 <- IRSAR.RF.Data

  ## simple test
  expect_s4_class(merge_RLum(list(o1,o2)), "RLum.Analysis")

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

  ## crash wit non-list
  expect_error(merge_RLum("errr"), "\\[merge\\_RLum\\(\\)\\] argument 'objects' .*")

})

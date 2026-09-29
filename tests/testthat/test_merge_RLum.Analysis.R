## load data
data(ExampleData.RLum.Analysis, envir = environment())
o1 <- IRSAR.RF.Data
c1 <- as(object = list(1:10), Class = "RLum.Data.Curve")
r1 <- as(object = list(1:10), Class = "RLum.Results")

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(merge_RLum(),
               "'object' should be of class 'list'")
  expect_error(merge_RLum(o1),
               "'object' should be of class 'list'")
  expect_error(merge_RLum(list(o1, "test")),
               "All elements of 'object' should be of class 'RLum.Analysis' or")
  expect_error(merge_RLum(list(o1, r1)),
               "All elements of 'object' should be of class 'RLum.Analysis' or")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  expect_snapshot_RLum(merge_RLum(list(o1)))
  expect_snapshot_RLum(merge_RLum(list(o1@records[[2]], o1, c1)))
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 1732
  expect_warning(res <- merge_RLum(list(set_RLum("RLum.Analysis"))),
                 "This 'RLum.Analysis' object has no records, NULL returned")
  expect_s4_class(res,
                  "RLum.Analysis")
  expect_length(res, 0)
})

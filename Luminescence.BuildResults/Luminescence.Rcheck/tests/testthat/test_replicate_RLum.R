test_that("Test replication of RLum-objects", {
  testthat::skip_on_cran()

  data(ExampleData.RLum.Analysis, envir = environment())
  expect_silent(results <- rep(IRSAR.RF.Data[[1]], 5))
  expect_silent(rep(IRSAR.RF.Data[[1]]))

  ##check
  expect_equal(length(results),5)

})

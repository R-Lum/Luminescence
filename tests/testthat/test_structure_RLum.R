## load data
data(ExampleData.RLum.Analysis, envir = environment())

test_that("snapshot tests", {
  testthat::skip_on_cran()

  expect_snapshot_plain(structure_RLum(IRSAR.RF.Data))
  expect_snapshot_plain(structure_RLum(list(IRSAR.RF.Data,
                                            IRSAR.RF.Data,
                                            "a")))

  ## empty object
  res <- structure_RLum(set_RLum("RLum.Analysis"))
  expect_snapshot_plain(res)
  expect_equal(nrow(res), 0)
})

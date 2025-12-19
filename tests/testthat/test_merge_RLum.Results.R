## load data
data(ExampleData.DeValues, envir = environment())
res <- calc_CentralDose(ExampleData.DeValues$CA1,
                        plot = FALSE, verbose = FALSE)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(merge_RLum.Results("error"),
               "'objects' should be of class 'list'")
  expect_error(merge_RLum.Results(list(res, "error")),
               "All elements of 'object' should be of class 'RLum.Results'")
  expect_message(expect_null(merge_RLum.Results(list())),
                 "'objects' contains no data, NULL returned")

  res2 <- res
  res2@originator <- "unknown"
  expect_error(merge_RLum.Results(list(res, res2)),
               "Objects cannot be merged, different originators found: 'calc_CentralDose', 'unknown'")

  res2 <- res
  res2@data[[1]][, 2] <- NULL
    expect_error(merge_RLum.Results(list(res, res2)),
               "Objects cannot be merged, different number of columns")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  set.seed(1)
  a <- array(runif(300, 0,255), c(10,10,3))
  roi <- matrix(c(2.,4,2,5,6,7,3,1,1), ncol = 3)
  expect_snapshot_RLum(merge_RLum.Results(lapply(list(roi, roi, roi),
                                                 function(x) extract_ROI(a, x))))

  expect_snapshot_RLum(merge_RLum.Results(list(res, res)))

  empty <- set_RLum("RLum.Results")
  expect_snapshot_RLum(merge_RLum.Results(list(empty)))
  expect_s4_class(merge_RLum.Results(list(empty, empty)),
                  "RLum.Results")
})

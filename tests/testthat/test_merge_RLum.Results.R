data(ExampleData.DeValues, envir = environment())
res <- calc_CentralDose(ExampleData.DeValues$CA1,
                        plot = FALSE, verbose = FALSE)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(merge_RLum.Results("error"),
               "'objects' has to be of type 'list'")
  expect_error(merge_RLum.Results(list(res, "error")),
               "All objects to be merged must have type 'RLum.Results'")

  res2 <- res
  res2@originator <- "unknown"
  expect_error(merge_RLum.Results(list(res, res2)),
               "Objects cannot be merged, different 'RLum.Results' originators found")

  res2 <- res
  res2@data[[1]][, 2] <- NULL
    expect_error(merge_RLum.Results(list(res, res2)),
               "Objects cannot be merged, different number of columns")
})

test_that("Merge RLum.Results", {
  testthat::skip_on_cran()

  ## check whether arguments are retained
  a <- array(runif(300, 0,255), c(10,10,3))
  roi <- matrix(c(2.,4,2,5,6,7,3,1,1), ncol = 3)
  t <-
    expect_s4_class(merge_RLum.Results(lapply(list(roi, roi, roi), function(x)
      extract_ROI(a, x))), "RLum.Results")

  expect_length(names(attributes(t@data$roi_summary)), 4)

  a <- merge_RLum.Results(list(res, res))
  expect_s3_class(a@data$summary, "data.frame")


})

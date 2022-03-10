test_that("Merge RLum.Results", {
  testthat::skip_on_cran()
  local_edition(3)

  ## check whether arguments are retained
  a <- array(runif(300, 0,255), c(10,10,3))
  roi <- matrix(c(2.,4,2,5,6,7,3,1,1), ncol = 3)
  t <-
    expect_s4_class(merge_RLum.Results(lapply(list(roi, roi, roi), function(x)
      extract_ROI(a, x))), "RLum.Results")

  expect_length(names(attributes(t@data$roi_summary)), 4)

  ## check standard without attributes to make sure that
  ## standard attributes become not removed
  ##load example data
  data(ExampleData.DeValues, envir = environment())

  ##apply the central dose model
  c <- calc_CentralDose(ExampleData.DeValues$CA1, plot = FALSE)
  a <- merge_RLum.Results(list(c,c))
  expect_s3_class(a@data$summary, "data.frame")


})

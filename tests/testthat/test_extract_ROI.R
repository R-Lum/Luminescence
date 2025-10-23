## load data
data(ExampleData.RLum.Data.Image, envir = environment())

## generate random data
set.seed(1)
m <- matrix(runif(100,0,255), ncol = 10, nrow = 10)
a <- array(runif(300, 0,255), c(10,10,3))
RLum <- set_RLum("RLum.Data.Image", data = a)
RLum_list <- list(RLum, RLum)
roi <- matrix(c(2.,4,2,5,6,7,3,1,1), ncol = 3)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(extract_ROI(object = "error", roi),
               "[extract_ROI()] 'object' should be of class 'RLum.Data.Image'",
               fixed = TRUE)
  expect_error(extract_ROI(object = m, "error"),
               "[extract_ROI()] 'roi' should be of class 'matrix'",
               fixed = TRUE)
  expect_error(extract_ROI(object = m, matrix()),
               "[extract_ROI()] 'roi' does not have the expected format",
               fixed = TRUE)
  expect_error(extract_ROI(object = m, matrix(ncol = 3, nrow = 0)),
               "[extract_ROI()] 'roi' does not have the expected format",
               fixed = TRUE)
  expect_error(extract_ROI(object = RLum, roi, roi_summary = "error"),
               "[extract_ROI()] 'roi_summary' should be one of 'mean', 'median'",
               fixed = TRUE)
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6

  ## matrix
  expect_snapshot_RLum(extract_ROI(object = m, roi),
                       tolerance = snapshot.tolerance)

  ## list
  expect_snapshot_RLum(extract_ROI(object = RLum_list, roi),
                       tolerance = snapshot.tolerance)

  ## test ROI summary options
  expect_snapshot_plain(extract_ROI(m, roi,
                                    roi_summary = "mean")@data$roi_summary)
  expect_snapshot_plain(extract_ROI(RLum, roi,
                                    roi_summary = "median")@data$roi_summary)
  expect_snapshot_plain(extract_ROI(RLum, roi,
                                    roi_summary = "sd")@data$roi_summary)
  expect_snapshot_plain(extract_ROI(RLum, roi,
                                    roi_summary = "sum")@data$roi_summary)
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  SW({
  vdiffr::expect_doppelganger("default",
                              extract_ROI(RLum,
                                          roi,
                                          plot = TRUE))
  vdiffr::expect_doppelganger("list",
                              extract_ROI(RLum_list,
                                          roi + 2,
                                          plot = TRUE))
  vdiffr::expect_doppelganger("image",
                              extract_ROI(ExampleData.RLum.Data.Image,
                                          matrix(c(200, 400, 200, 40, 60, 80,
                                                   10, 10, 10), ncol = 3),
                                          plot = TRUE))
  })
})

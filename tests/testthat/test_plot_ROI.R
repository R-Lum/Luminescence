## load dataset
temp <- read_RF2R(system.file("extdata", "RF_file.rf", package = "Luminescence"),
                  verbose = FALSE)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_ROI(object = "stop"),
               "[plot_ROI()] 'object' should be of class 'RLum.Analysis'",
               fixed = TRUE)

  empty <- set_RLum("RLum.Results", originator = NA_character_)
  expect_error(plot_ROI(empty),
               "Object originator 'NA' not supported")
  expect_error(plot_ROI(list()),
               "'object' cannot be an empty list")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ##test standard cases
  expect_silent(plot_ROI(temp))
  expect_silent(plot_ROI(temp, grid = TRUE))
  expect_silent(plot_ROI(temp, dim.CCD = c(8192,8192)))
  expect_silent(plot_ROI(temp, dist_thre = 20))
  expect_silent(plot_ROI(temp, exclude_ROI = NULL))

  ##test non-list case
  expect_silent(plot_ROI(temp[[1]]))
  expect_silent(plot_ROI(temp[[1]], exclude_ROI = NULL))

  ##output only case
  expect_s4_class(plot_ROI(temp, plot = FALSE), class = "RLum.Results")

  ## test combination with extract_ROI()
  m <- matrix(runif(100,0,255), ncol = 10, nrow = 10)
  roi <- matrix(c(2.,4,2,5,6,7,3,1,1), ncol = 3)
  t <- extract_ROI(object = m, roi = roi)
  expect_s4_class(plot_ROI(t, bg_image = m, exclude_ROI = NULL), "RLum.Results")

  ## trigger warning
  expect_warning(plot_ROI(t, bg_image = "stop", exclude_ROI = NULL),
                 "[plot_ROI()] 'bg_image' is not of class 'RLum.Data.Image'",
                 fixed = TRUE)
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not(getRversion() >= "4.4.0")

  SW({
  vdiffr::expect_doppelganger("ROI defaults",
                              plot_ROI(temp))
  vdiffr::expect_doppelganger("ROI grid",
                              plot_ROI(temp, grid = TRUE))

  set.seed(1)
  m <- matrix(runif(100, 0, 255), ncol = 10, nrow = 10)
  roi <- matrix(c(2, 4, 2, 5, 6, 7, 3, 1, 1), ncol = 3)
  t <- extract_ROI(object = m, roi = roi)
  vdiffr::expect_doppelganger("ROI bg_image",
                              plot_ROI(t, bg_image = m, exclude_ROI = NULL))
  })
})

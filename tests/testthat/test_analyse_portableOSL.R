## load data
data(ExampleData.portableOSL, envir = environment())

## generate test data set for profile
merged <- surface <- merge_RLum(ExampleData.portableOSL)

## generate dataset for surface: each sample has its own set of coordinates
sample.names <- unique(sapply(surface@records, function(x) x@info$settings$Sample))
surface@records <- lapply(surface@records, function(x){
  set.seed(match(x@info$settings$Sample, sample.names))
  x@info$settings$Sample <- paste0("Test_x:", runif(1), "|y:", runif(1))
  x
})

test_that("check class and length of output", {
    testthat::skip_on_cran()

    ## standard run profile
    results <- expect_s4_class(
      analyse_portableOSL(
        merged,
        signal.integral = 1:5,
        invert = FALSE,
        mode = "profile",
        normalise = TRUE,
        plot = TRUE
      ), "RLum.Results")

    ## check standard with coordinates
    coord <- as.matrix(results$summary[,c("COORD_X", "COORD_Y")])

    ## verify output
      expect_equal(length(results), 3)
      expect_s3_class(results$summary, "data.frame")
      expect_s4_class(results$data, "RLum.Analysis")
    expect_equal(round(sum(results$summary[,c(-1, -2, -10,-11)]), digits = 2), 175.44)

    ## standard surface
    results <- expect_s4_class(
        analyse_portableOSL(
          surface,
          signal.integral = 1:5,
          invert = FALSE,
          mode = "surface",
          normalise = TRUE,
          plot = TRUE
        ), "RLum.Results")

    ## surface with options
    set.seed(1234)
    results <- expect_s4_class(
      analyse_portableOSL(
        surface,
        signal.integral = 1:5,
        invert = TRUE,
        mode = "surface",
        xlim = c(0.1, 0.6),
        ylim = c(0.1, 0.8),
        zlim = c(0.1, 2),
        zlim_image = c(1, 2),
        col_ramp = grDevices::topo.colors(20),
        surface_value = c("BSL", "IRSL"),
        normalise = TRUE,
        contour = TRUE,
        contour_nlevels = 20,
        plot = TRUE
      ), "RLum.Results")

    ## check list input
    expect_s4_class(
      suppressWarnings(analyse_portableOSL(list(surface), mode = "surface")),
      "RLum.Results")
    expect_warning(expect_null(analyse_portableOSL(list())),
                   "Nothing was merged as the object list was found to be empty")

    ## check additional argument sample
    expect_s4_class(analyse_portableOSL(
      merged,
      signal.integral = 1:5,
      invert = FALSE,
      normalise = TRUE,
      ylim = c(1,2),
      zlim = list(BSL = c(0,1.1), IRSL = c(0,1)),
      plot = TRUE,
      sample = "test"
    ),
    "RLum.Results")

    ## more coverage
    expect_s4_class(analyse_portableOSL(
        surface,
        signal.integral = 1:5,
        mode = "surface",
        bg_img = as.raster(matrix(0:1, ncol = 4, nrow = 3))
    ), "RLum.Results")
})

test_that("input validation", {
    testthat::skip_on_cran()

    expect_error(analyse_portableOSL("error"),
                 "[analyse_portableOSL()] 'object' should be of class 'RLum.Analysis'",
                 fixed = TRUE)
    expect_error(analyse_portableOSL(set_RLum("RLum.Analysis")),
                 "'object' cannot be an empty RLum.Analysis")
    expect_error(analyse_portableOSL(merged, signal.integral = 1:5,
                                     mode = "error"),
                 "'mode' should be one of 'profile' or 'surface'")

    ## Only RLum.Data.Curves
    tmp <- merged
    tmp@records <- list(tmp@records, "error")
    expect_error(analyse_portableOSL(tmp),
                 regexp = "\\[analyse\\_portableOSL\\(\\)\\] The 'RLum.Analysis' object must contain only.+")

    ## Check originator
    tmp <- merged
    tmp@records[[1]]@originator <- "error"
    expect_error(analyse_portableOSL(tmp),
                 regexp = "\\[analyse\\_portableOSL\\(\\)\\] Only objects originating from .+")

    ## Sequence pattern
    expect_error(analyse_portableOSL(merged[-7], signal.integral = 1:5),
                 "Sequence pattern not supported: see the manual for details")
    expect_error(analyse_portableOSL(merged[1:3], signal.integral = 1:3),
                 "Sequence pattern not supported: see the manual for details")
    expect_error(analyse_portableOSL(merged[c(1:5, rep(7, 5))],
                                     signal.integral = 1:5),
                 "Sequence pattern not supported: the number of OSL records")
    expect_error(analyse_portableOSL(merged[c(1:5, rep(6, 5))],
                                     signal.integral = 1:5),
                 "Sequence pattern not supported: expected 3 DARK_COUNT records")
    expect_error(analyse_portableOSL(merged[-c(7:11)], signal.integral = 1:5),
                 "'object' references 14 sample names, but only 13 IRSL/OSL")

    ## coordinates not list or matrix
    expect_error(analyse_portableOSL(surface, signal.integral = 1:5,
                                     coord = "error"),
      "'coord' should be of class 'matrix' or 'list'")

    ## coordinates are not of the correct size
    expect_error(analyse_portableOSL(surface, signal.integral = 1:5,
                                     coord = list(COORD_X = c(0, 0),
                                                  COORD_Y = c(1, 2))),
                 "The number of coordinates in 'coord' should match the number",
                 fixed = TRUE)

    ## trigger warning
    expect_message(
      analyse_portableOSL(
        merged,
        signal.integral = 1:5,
        invert = FALSE,
        normalise = TRUE,
        mode = "surface",
        surface_value = c("BSL"),
        plot = TRUE,
        sample = "test"),
      "Surface plot is not available when all x-coordinates are 0")

    expect_error(
      analyse_portableOSL(
        surface,
        signal.integral = 1:5,
        mode = "surface",
        surface_value = "error"),
      "Unknown value to plot, valid values are:")

    expect_warning(
      analyse_portableOSL(
        merged,
        signal.integral = 1:5,
        invert = FALSE,
        normalise = TRUE,
        mode = "profile",
        zlim = c(1,2),
        plot = TRUE,
        sample = "test"),
      regexp = "\\[analyse\\_portableOSL\\(\\)\\] In profile mode, zlim.+")

    expect_warning(analyse_portableOSL(merged[1:5],
                                       signal.integral = c(1, 102)),
                   "'signal.integral' (1, 102) exceeds the number of data points",
                   fixed = TRUE)
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not(getRversion() >= "4.4.0")

  SW({
  vdiffr::expect_doppelganger("profile",
                              analyse_portableOSL(merged, mode = "profile",
                                                  signal.integral = 1:5))
  })
})

test_that("regression tests", {
  testthat::skip_on_cran()

  SW({
  ## issue 675
  expect_warning(analyse_portableOSL(ExampleData.portableOSL[[1]],
                                     coord = list(c(1, 1)),
                                     signal.integral = 1:5, mode = "surface"),
                 "Surface interpolation failed: this happens when all points")

  ## issue 680
  expect_warning(analyse_portableOSL(ExampleData.portableOSL[[1]],
                                     signal.integral = c(-3, 200)),
                 "exceeds the number of data points, reset to (1, 100)",
                 fixed = TRUE)
  })
})

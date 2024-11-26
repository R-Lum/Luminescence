## load data
data(ExampleData.portableOSL, envir = environment())

## generate test data set for profile
merged <- surface <- merge_RLum(ExampleData.portableOSL)

## generate dataset for surface
surface@records <- lapply(surface@records, function(x){
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

    ## standard run profile no plot even with plot activated
    results <- expect_s4_class(
      analyse_portableOSL(
        merged,
        signal.integral = 1:5,
        invert = FALSE,
        normalise = TRUE,
        mode = NULL,
        coord = coord,
        plot = TRUE
      ), "RLum.Results")

    ## verify output
      expect_equal(length(results), 3)
      expect_s3_class(results$summary, "data.frame")
      expect_s4_class(results$data, "RLum.Analysis")

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
        ylim = c(0.1, 0.6),
        zlim = c(0.1, 2),
        zlim_image = c(1, 2),
        col_ramp = grDevices::topo.colors(20),
        surface_values = c("BSL", "IRSL"),
        normalise = TRUE,
        contour = TRUE,
        plot = TRUE
      ), "RLum.Results")

    ## check list input
    expect_s4_class(
      suppressWarnings(analyse_portableOSL(ExampleData.portableOSL)),
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
    tmp <- merged
    tmp@records <- tmp@records[-1]
    expect_error(analyse_portableOSL(tmp),
                 "Sequence pattern not supported")

    ## coordinates not list or matrix
    expect_error(analyse_portableOSL(surface, signal.integral = 1:5,
                                     coord = "error"),
      "'coord' should be of class 'matrix' or 'list'")

    ## coordinates are not of the correct size
    expect_error(analyse_portableOSL(surface, signal.integral = 1:5,
                                     coord = list(COORD_X = c(0, 0),
                                                  COORD_Y = c(1, 2))),
                 "Number of coordinates differ from the number of samples")

    ## trigger warning
    expect_warning(
      analyse_portableOSL(
        merged,
        signal.integral = 1:5,
        invert = FALSE,
        normalise = TRUE,
        mode = "surface",
        surface_value = c("BSL"),
        plot = TRUE,
        sample = "test"),
      "Surface interpolation failed: this happens when all points are")

    expect_error(
      analyse_portableOSL(
        merged,
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

    suppressWarnings( # generated the same warning twice
    expect_warning(analyse_portableOSL(merged[1:5],
                                       signal.integral = c(1, 102)),
                   "'signal.integral' (1, 102) exceeded the number",
                   fixed = TRUE)
    )
})

test_that("check output", {
  testthat::skip_on_cran()

  data("ExampleData.portableOSL", envir = environment())
  merged <- merge_RLum(ExampleData.portableOSL)
  results <-
    analyse_portableOSL(
      merged,
      signal.integral = 1:5,
      invert = FALSE,
      normalise = TRUE,
      plot = FALSE
    )

  expect_equal(round(sum(results$summary[,c(-1, -2, -10,-11)]), digits = 2), 175.44)
})

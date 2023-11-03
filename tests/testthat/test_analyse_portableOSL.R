test_that("check class and length of output", {
    testthat::skip_on_cran()
    local_edition(3)

    ## generate test data set for profile
    data("ExampleData.portableOSL", envir = environment())
    merged <- surface <- merge_RLum(ExampleData.portableOSL)

    ## generate dataset for surface
    surface@records <- lapply(surface@records, function(x){
      x@info$settings$Sample <- paste0("Test_x:", runif(1), "|y:", runif(1))
      x
    })

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
        xlim = c(0.1, 0.4),
        ylim = c(0.1, 0.4),
        zlim = c(0.1, 2),
        col_ramp = "red",
        surface_values = c("BSL", "IRSL"),
        normalise = TRUE,
        plot = TRUE
      ), "RLum.Results")

    ## check list input
    expect_s4_class(
      suppressWarnings(analyse_portableOSL(ExampleData.portableOSL)),
      "RLum.Results")

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

    ## trigger stops
    ## Only RLum.Analysis
    expect_error(analyse_portableOSL("error"),
                 regexp = "\\[analyse\\_portableOSL\\(\\)\\] Only objects of class.+")

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
    expect_error(
      object = analyse_portableOSL(tmp),
      regexp = "\\[analyse\\_portableOSL\\(\\)\\] Sequence pattern not supported.+")

    ## coordinates not list or matrix
    expect_error(
      analyse_portableOSL(
        surface,
        signal.integral = 1:5,
        invert = FALSE,
        mode = "surface",
        coord = "error",
        normalise = TRUE,
        plot = FALSE),
      regexp = "\\[analyse\\_portableOSL\\(\\)\\] Argument 'coord' needs to be a.+")

    ## coordinates are not of the correct size
    expect_error(
      analyse_portableOSL(
        surface,
        signal.integral = 1:5,
        invert = FALSE,
        mode = "surface",
        coord = coord[1:2,],
        normalise = TRUE,
        plot = FALSE),
      regexp = "\\[analyse\\_portableOSL\\(\\)\\] Number of coordinates differ from the number.+")

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
      regexp = "\\[analyse\\_portableOSL\\(\\)\\] Surface interpolation failed, this.+")

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

})

test_that("check output", {
  testthat::skip_on_cran()
  local_edition(3)

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

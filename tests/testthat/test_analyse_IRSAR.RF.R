## load data
data(ExampleData.RLum.Analysis, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(analyse_IRSAR.RF(),
               "is missing, with no default")
  expect_error(analyse_IRSAR.RF("test"),
               "'object' should be of class 'RLum.Analysis'")
  expect_error(analyse_IRSAR.RF(iris),
               "'object' should be of class 'RLum.Analysis'")
  expect_error(analyse_IRSAR.RF(set_RLum("RLum.Analysis")),
               "'object' cannot be an empty RLum.Analysis")
  expect_error(analyse_IRSAR.RF(IRSAR.RF.Data, n.MC = 0),
               "'n.MC' should be a positive integer scalar")
  expect_error(analyse_IRSAR.RF(IRSAR.RF.Data, method = "error"),
               "'method' should be one of 'FIT', 'SLIDE', 'VSLIDE' or 'NONE'")
  expect_error(analyse_IRSAR.RF(IRSAR.RF.Data, method_control = 3),
               "'method_control' should be of class 'list'")
  expect_warning(expect_null(analyse_IRSAR.RF(list())),
                 "Nothing was merged as the object list was found to be empty")

  ## sequence_structure
  expect_error(analyse_IRSAR.RF(IRSAR.RF.Data, sequence_structure = FALSE),
               "'sequence_structure' should be of class 'character'")
  expect_error(analyse_IRSAR.RF(IRSAR.RF.Data, sequence_structure = ""),
               "'sequence_structure' should contain at least two elements")
  expect_error(analyse_IRSAR.RF(IRSAR.RF.Data,
                                sequence_struct = c("NATURAL", "NATURAL")),
               "'sequence_structure' must contain one each of 'NATURAL' and")
  expect_error(analyse_IRSAR.RF(IRSAR.RF.Data,
                                sequence_struct = c("REGENERATED", "REGENERATED")),
               "'sequence_structure' must contain one each of 'NATURAL' and")
  expect_error(analyse_IRSAR.RF(IRSAR.RF.Data,
                                sequence_struct = c("NATURAL", "NATURAL", "REGENERATED")),
               "'sequence_structure' is missing one of 'NATURAL' or")
  expect_error(analyse_IRSAR.RF(IRSAR.RF.Data,
                                sequence_struct = c("REGENERATED", "NATURAL")),
               "Number of data channels in RF_nat > RF_reg")

  ## RF_nat.lim
  expect_error(analyse_IRSAR.RF(IRSAR.RF.Data, RF_nat.lim = "error"),
               "'RF_nat.lim' should be of class 'numeric' or 'integer'")
  expect_warning(analyse_IRSAR.RF(IRSAR.RF.Data, RF_nat.lim = 6),
                 "'RF_nat.lim' out of bounds, reset to")

  ## RF_reg.lim
  expect_error(analyse_IRSAR.RF(IRSAR.RF.Data, RF_reg.lim = "error"),
               "'RF_reg.lim' should be of class 'numeric' or 'integer'")
  expect_warning(analyse_IRSAR.RF(IRSAR.RF.Data, RF_reg.lim = 2000),
                 "'RF_reg.lim' out of bounds, reset to")
  expect_error(analyse_IRSAR.RF(IRSAR.RF.Data, RF_reg.lim = 521),
               "'RF_reg.lim' defines too short an interval and it's not")
  expect_error(analyse_IRSAR.RF(IRSAR.RF.Data, RF_reg.lim = 520),
               "The range of regenerated channels should be larger than")
  suppressWarnings( # FIXME(mcol): lmdif: info = -1. Number of iterations has reached `maxiter' == 50.
  expect_warning(analyse_IRSAR.RF(IRSAR.RF.Data, RF_reg.lim = c(3, 6)),
                 "'RF_reg.lim' defines too short an interval, reset to")
  )

  ## curves of the same length
  expect_error(analyse_IRSAR.RF(set_RLum("RLum.Analysis",
                                         records = list(IRSAR.RF.Data[[1]],
                                                        IRSAR.RF.Data[[1]])),
                                method = "VSLIDE"),
               "There is no further sliding space left")

  SW({
  expect_warning(analyse_IRSAR.RF(IRSAR.RF.Data,
                                  method_control = list(unknown = "test")),
                 "'unknown' not supported for 'method_control'")

  ## disable test that produces this error on CI:
  ## Error in `.check_ncores(length(names))`: 4 simultaneous processes spawned
  if (FALSE) {
  expect_warning(analyse_IRSAR.RF(IRSAR.RF.Data, method = "VSLIDE",
                                  method_control = list(cores = 10000)),
                 "Number of cores limited to the maximum available")
  }

  ## vslide_range
  expect_error(analyse_IRSAR.RF(IRSAR.RF.Data, method = "VSLIDE",
                                method_control = list(vslide_range = FALSE)),
                 "'vslide_range' in 'method_control' should be of class")

  expect_error(analyse_IRSAR.RF(IRSAR.RF.Data, method = "VSLIDE",
                                method_control = list(vslide_range = "error")),
                 "'vslide_range' in 'method_control' should be either 'auto'")
  expect_warning(analyse_IRSAR.RF(IRSAR.RF.Data, method = "VSLIDE",
                                  method_control = list(vslide_range = 1:4)),
                 "'vslide_range' in 'method_control' has more than 2 elements")

  ## num_slide_windows
  expect_error(analyse_IRSAR.RF(IRSAR.RF.Data, method = "VSLIDE",
                                method_control = list(num_slide_windows = NA)),
                 "'num_slide_windows' in 'method_control' should be a positive")
  expect_warning(analyse_IRSAR.RF(IRSAR.RF.Data, method = "VSLIDE",
                                  method_control = list(num_slide_windows = 20)),
                 "should be between 1 and 10, reset to 10")

  expect_message(analyse_IRSAR.RF(IRSAR.RF.Data, method = "VSLIDE",
                                  method_control = list(cores = "4")),
                 "Invalid value for control argument 'cores'")
  })

  ## deprecated option
  expect_warning(analyse_IRSAR.RF(IRSAR.RF.Data, verbose = FALSE,
                                  method.control = list(cores = 1)),
                 "'method.control' is deprecated, use 'method_control'")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  set.seed(1)
  expect_snapshot_RLum(
      results_fit <- analyse_IRSAR.RF(IRSAR.RF.Data, method = "FIT",
                                      plot = TRUE))
  expect_snapshot_RLum(
      results_slide <- analyse_IRSAR.RF(IRSAR.RF.Data, method = "SLIDE",
                                        plot = TRUE, n.MC = NULL))
  SW({
  expect_snapshot_RLum(
  results_slide_alt <-
    analyse_IRSAR.RF(
      object = IRSAR.RF.Data,
      plot = FALSE,
      method = "SLIDE",
      n.MC = 10,
      method_control = list(vslide_range = 'auto', trace_vslide = TRUE,
                            num_slide_windows = 10),
      txtProgressBar = FALSE
    )
  )

  expect_snapshot_RLum(
  results_slide_alt2 <-
    analyse_IRSAR.RF(
      object = IRSAR.RF.Data,
      plot = FALSE,
      method = "VSLIDE",
      n.MC = 10,
      method_control = list(vslide_range = 'auto', trace_vslide = FALSE,
                            num_slide_windows = 10),
      txtProgressBar = FALSE
    )
  )
  })

  expect_snapshot_RLum(
    analyse_IRSAR.RF(
      object = IRSAR.RF.Data,
      method = "None",
      mtext = "Subtitle",
      n.MC = 10,
      txtProgressBar = FALSE
    )
  )

  expect_s3_class(results_fit$fit, class = "nls")
  expect_s3_class(results_slide$fit, class = "nls")
})

test_that("test controlled crash conditions", {
  testthat::skip_on_cran()

  ##the sliding range should not exceed a certain value ... test it
  expect_error(
    analyse_IRSAR.RF(
      object = IRSAR.RF.Data,
      method = "SLIDE",
      method_control = list(vslide_range = c(0,1e+07)),
    ), regexp = "[:::src_analyse_IRSAR_SRS()] 'vslide_range' exceeded maximum size (1e+07)!", fixed = TRUE)
})

test_that("test support for IR-RF data", {
  testthat::skip_on_cran()

  ## get needed data
  file <- system.file("extdata", "RF_file.rf", package = "Luminescence")
  temp <- read_RF2R(file, verbose = FALSE)

  SW({
  expect_warning(expect_s4_class(
      analyse_IRSAR.RF(object = temp[1:3], method = "SLIDE",
                       cex = 1.1, xlim = c(750, 9000), ylim = c(640, 655),
                       method_control = list(show_fit = TRUE),
                       plot_reduced = TRUE, n.MC = 1),
      "RLum.Results"),
      "Narrow density distribution, no density distribution plotted")
  })
})

test_that("test edge cases", {
  testthat::skip_on_cran()

  data(ExampleData.RLum.Analysis, envir = environment())
  RF_nat <- RF_reg <- IRSAR.RF.Data[[2]]
  RF_reg@data[,2] <- runif(length(RF_reg@data[,2]), 0.007557956, 0.05377426 )
  RF_nat@data[,2] <- runif(length(RF_nat@data[,2]), 65.4, 76.7)
  RF_nat@data <- RF_nat@data[1:50,]
  RF_nat@info <- list(startDate = "20210101150845")
  object <- set_RLum("RLum.Analysis", records = list(RF_nat, RF_reg))

  SW({
  expect_warning(analyse_IRSAR.RF(
    IRSAR.RF.Data,
    method = "FIT",
    plot = TRUE,
    RF_reg = c(1, 400),
    txtProgressBar = FALSE),
    "Threshold exceeded for: 'curves_bounds'")

  expect_warning(expect_s4_class(analyse_IRSAR.RF(
    list(object),
    method = "SLIDE",
    method_control = list(vslide_range = 'auto', correct_onset = FALSE,
                          show_fit = TRUE, trace = TRUE, n.MC = 2,
                          cores = 2),
    RF_nat.lim = 2,
    RF_reg.lim = 2,
    plot = TRUE,
    main = "Title",
    mtext = "Subtitle",
    log = "x",
    txtProgressBar = FALSE),
    "RLum.Results"),
    "Threshold exceeded for: 'curves_ratio'")
  })

  ## this RF_nat.lim after
  ##  'length = 2' in coercion to 'logical(1)' error
  expect_s4_class(suppressWarnings(analyse_IRSAR.RF(
    object,
    method = "SLIDE",
    method_control = list(vslide_range = 'auto', correct_onset = FALSE),
    RF_nat.lim = c(10,100),
    #RF_reg.lim = c(),
    plot = TRUE,
    txtProgressBar = FALSE
  )), "RLum.Results")

  expect_s4_class(suppressWarnings(analyse_IRSAR.RF(
    object,
    method = "FIT",
    mtext = "FIT method",
    plot = TRUE,
    txtProgressBar = FALSE
  )), "RLum.Results")

  ## test parameters values only set for coverage
  SW({
  expect_s4_class(analyse_IRSAR.RF(
    object,
    method = "SLIDE",
    method_control = list(vslide_range = 'auto', correct_onset = FALSE),
    #RF_nat.lim = c(10,100),
    RF_reg.lim = c(10,100),
    plot = TRUE,
    test_parameters = list(curves_ratio = NULL,
                           intersection_ratio = 5,
                           residuals_slope = 1,
                           dynamic_ratio = 1,
                           lambda = 0,
                           beta = 1e-4,
                           delta.phi = 1e-4),
    txtProgressBar = FALSE
  ), "RLum.Results")
  })

  ## more coverage
  expect_s4_class(analyse_IRSAR.RF(
      IRSAR.RF.Data,
      method = "SLIDE",
      RF_reg.lim = c(1, 7),
      verbose = FALSE),
      "RLum.Results")

  tmp <- IRSAR.RF.Data
  tmp@records[[2]]@data <- tmp@records[[2]]@data[1:460, ]
  expect_s4_class(analyse_IRSAR.RF(
      tmp,
      method = "SLIDE",
      verbose = FALSE),
      "RLum.Results")
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 372
  expect_silent(analyse_IRSAR.RF(list(IRSAR.RF.Data), plot = FALSE,
                                 method_control = list(maxiter = 10)))

  ## issue 382
  expect_silent(analyse_IRSAR.RF(list(IRSAR.RF.Data), plot = FALSE))

  ## issue 816
  data(ExampleData.portableOSL, envir = environment())
  expect_silent(analyse_IRSAR.RF(merge_RLum(ExampleData.portableOSL)[1:4],
                                 plot = FALSE))
  expect_silent(analyse_IRSAR.RF(merge_RLum(ExampleData.portableOSL)[1:9],
                                 sequence_structure = c("NATURAL", rep("REGENERATED", 2)),
                                 plot = FALSE))
})

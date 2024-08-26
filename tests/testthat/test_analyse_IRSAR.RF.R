data(ExampleData.RLum.Analysis, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(analyse_IRSAR.RF(),
               "is missing, with no default")
  expect_error(analyse_IRSAR.RF("test"),
               "Input object must be of type 'RLum.Analysis'")
  expect_error(analyse_IRSAR.RF(IRSAR.RF.Data, sequence_structure = FALSE),
               "'sequence_structure' must be of type 'character'")
  expect_error(analyse_IRSAR.RF(IRSAR.RF.Data, n.MC = 0),
               "'n.MC' must be a positive integer scalar")
  expect_error(analyse_IRSAR.RF(IRSAR.RF.Data, method.control = 3),
               "'method.control' has to be of type 'list'")

  SW({
  expect_warning(analyse_IRSAR.RF(IRSAR.RF.Data,
                                  method.control = list(unknown = "test")),
                 "'unknown' not supported for 'method.control'")

  ## disable test that produces this error on CI:
  ## Error in `.check_ncores(length(names))`: 4 simultaneous processes spawned
  if (FALSE) {
  expect_warning(analyse_IRSAR.RF(IRSAR.RF.Data, method = "VSLIDE",
                                  method.control = list(cores = 10000)),
                 "Your machine has only [0-9]* cores")
  }

  expect_warning(analyse_IRSAR.RF(IRSAR.RF.Data, method = "VSLIDE",
                                  method.control = list(vslide_range = 1:4)),
                 "'vslide_range' in 'method.control' has more than 2 elements")

  expect_message(analyse_IRSAR.RF(IRSAR.RF.Data, method = "VSLIDE",
                                  method.control = list(cores = "4")),
                 "Invalid value for control argument 'cores'")

  expect_warning(analyse_IRSAR.RF(IRSAR.RF.Data, method = "UNKNOWN"),
                 "Analysis skipped: Unknown method or threshold of test")
  })
})

test_that("check class and length of output", {
  testthat::skip_on_cran()

  set.seed(1)
  SW({
  results_fit <- analyse_IRSAR.RF(object = IRSAR.RF.Data, plot = TRUE, method = "FIT")
  results_slide <- suppressWarnings(
    analyse_IRSAR.RF(object = IRSAR.RF.Data, plot = TRUE, method = "SLIDE", n.MC = NULL))
  results_slide_alt <- expect_s4_class(
    analyse_IRSAR.RF(
      object = IRSAR.RF.Data,
      plot = FALSE,
      method = "SLIDE",
      n.MC = 10,
      method.control = list(vslide_range = 'auto', trace_vslide = TRUE),
      txtProgressBar = FALSE
    ), class = "RLum.Results")

  results_slide_alt2 <- expect_s4_class(
    analyse_IRSAR.RF(
      object = IRSAR.RF.Data,
      plot = FALSE,
      method = "VSLIDE",
      n.MC = 10,
      method.control = list(vslide_range = 'auto', trace_vslide = FALSE),
      txtProgressBar = FALSE
    ), class = "RLum.Results")
  })

  expect_equal(is(results_fit), c("RLum.Results", "RLum"))
  expect_equal(length(results_fit), 5)
  expect_equal(length(results_slide), 5)
  expect_s3_class(results_fit$fit, class = "nls")
  expect_s3_class(results_slide$fit, class = "nls")
  expect_length(results_slide$slide, 10)

  expect_equal(results_fit$data$DE, 623.25)
  expect_equal(results_fit$data$DE.LOWER, 600.63)
  expect_equal(results_slide$data$DE, 610.17)
  expect_equal(round(results_slide_alt$data$DE, digits = 0), 384)
  expect_equal(round(results_slide_alt2$data$DE, digits = 0), 384)

})

test_that("test controlled crash conditions", {
  testthat::skip_on_cran()

  ##the sliding range should not exceed a certain value ... test it
  expect_error(
    analyse_IRSAR.RF(
      object = IRSAR.RF.Data,
      method = "SLIDE",
      method.control = list(vslide_range = c(0,1e+07)),
    ), regexp = "[:::src_analyse_IRSAR_SRS()] 'vslide_range' exceeded maximum size (1e+07)!", fixed = TRUE)
})

test_that("test support for IR-RF data", {
  testthat::skip_on_cran()

  ## get needed data
  file <- system.file("extdata", "RF_file.rf", package = "Luminescence")
  temp <- read_RF2R(file)

  SW({
  expect_warning(expect_s4_class(
      analyse_IRSAR.RF(object = temp[1:3], method = "SLIDE",
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

  SW({
  expect_s4_class(suppressWarnings(analyse_IRSAR.RF(
    set_RLum("RLum.Analysis", records = list(RF_nat, RF_reg)),
    method = "SLIDE",
    method.control = list(vslide_range = 'auto', correct_onset = FALSE,
                          show_fit = TRUE, trace = TRUE),
    RF_nat.lim = 2,
    RF_reg.lim = 2,
    plot = TRUE,
    mtext = "Subtitle",
    txtProgressBar = FALSE
  )), "RLum.Results")
  })

  ## this RF_nat.lim after
  ##  'length = 2' in coercion to 'logical(1)' error
  expect_s4_class(suppressWarnings(analyse_IRSAR.RF(
    set_RLum("RLum.Analysis", records = list(RF_nat, RF_reg)),
    method = "SLIDE",
    method.control = list(vslide_range = 'auto', correct_onset = FALSE),
    RF_nat.lim = c(10,100),
    #RF_reg.lim = c(),
    plot = TRUE,
    txtProgressBar = FALSE
  )), "RLum.Results")

  expect_s4_class(suppressWarnings(analyse_IRSAR.RF(
    set_RLum("RLum.Analysis", records = list(RF_nat, RF_reg)),
    method = "SLIDE",
    method.control = list(vslide_range = 'auto', correct_onset = FALSE),
    #RF_nat.lim = c(10,100),
    RF_reg.lim = c(10,100),
    plot = TRUE,
    txtProgressBar = FALSE
  )), "RLum.Results")

})

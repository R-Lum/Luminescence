test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

  set.seed(1)
  data(ExampleData.RLum.Analysis, envir = environment())
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
  local_edition(3)

  ##the sliding range should not exceed a certain value ... test it
  data(ExampleData.RLum.Analysis, envir = environment())
  expect_error(
    analyse_IRSAR.RF(
      object = IRSAR.RF.Data,
      plot = FALSE,
      method = "SLIDE",
      n.MC = 2,
      method.control = list(vslide_range = c(0,1e+07)),
      txtProgressBar = FALSE
    ), regexp = "[:::src_analyse_IRSAR_SRS()] 'vslide_range' exceeded maximum size (1e+07)!", fixed = TRUE)


  ## test multi-core error
  ## This does not seem to work on the GitHub Actions platform
  # expect_warning(
  #   analyse_IRSAR.RF(
  #     object = IRSAR.RF.Data,
  #     plot = FALSE,
  #     method = "VSLIDE",
  #     n.MC = 2,
  #     method.control = list(cores = 10000),
  #     txtProgressBar = FALSE
  #   ), regexp = "\\[analyse\\_IRSAR.RF\\(\\)] What do you want\\?")

  ## test wrong input for multicore
  ## test multi-core error
  expect_message(
    analyse_IRSAR.RF(
      object = IRSAR.RF.Data,
      plot = FALSE,
      method = "VSLIDE",
      n.MC = 2,
      method.control = list(cores = "4"),
      txtProgressBar = FALSE
    ), regexp = "\\[analyse\\_IRSAR\\.RF\\(\\)\\] Invalid value for control argument \\'cores\\'")


})

test_that("test support for IR-RF data", {
  testthat::skip_on_cran()
  local_edition(3)

  ## get needed data
  file <- system.file("extdata", "RF_file.rf", package = "Luminescence")
  temp <- read_RF2R(file)

  expect_s4_class(
    suppressWarnings(
      analyse_IRSAR.RF(object = temp[1:3], method = "SLIDE", plot_reduced = TRUE, n.MC = 1)),
    "RLum.Results")

})

test_that("test edge cases", {
  testthat::skip_on_cran()
  local_edition(3)

  data(ExampleData.RLum.Analysis, envir = environment())
  RF_nat <- RF_reg <- IRSAR.RF.Data[[2]]
  RF_reg@data[,2] <- runif(length(RF_reg@data[,2]), 0.007557956, 0.05377426 )
  RF_nat@data[,2] <- runif(length(RF_nat@data[,2]), 65.4, 76.7)
  RF_nat@data <- RF_nat@data[1:50,]

  expect_s4_class(suppressWarnings(analyse_IRSAR.RF(
    set_RLum("RLum.Analysis", records = list(RF_nat, RF_reg)),
    method = "SLIDE",
    method.control = list(vslide_range = 'auto', correct_onset = FALSE),
    RF_nat.lim = 2,
    RF_reg.lim = 2,
    plot = TRUE,
    txtProgressBar = FALSE
  )), "RLum.Results")

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

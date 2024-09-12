##(1) load package test data set
data(ExampleData.BINfileData, envir = environment())

##(2) selecting relevant curves, and limit dataset
CWOSL.sub <- subset(CWOSL.SAR.Data,
                    subset = POSITION %in% c(1:3) & LTYPE == "OSL")

test_that("input validation", {
  skip_on_cran()

  expect_error(analyse_baSAR("error", verbose = FALSE),
               "File does not exist")
  expect_error(analyse_baSAR(list("error"), verbose = FALSE),
               "File does not exist")
  expect_error(analyse_baSAR(data.frame(), verbose = FALSE),
               "'data.frame' as input is not supported")
  expect_error(analyse_baSAR(list(data.frame()), verbose = FALSE),
               "Unsupported data type in the input list provided for 'object'")
  expect_error(analyse_baSAR(list(data.frame(), matrix()), verbose = FALSE),
               "'object' only accepts a list with objects of similar type")
  expect_error(analyse_baSAR(CWOSL.sub, n.MCMC = NULL),
               "'n.MCMC' must be a positive integer scalar")

  expect_error(analyse_baSAR(CWOSL.sub, verbose = FALSE),
               "'source_doserate' is missing, but the current implementation")
  expect_error(analyse_baSAR(CWOSL.sub, fit.method = "error"),
               "'fit.method' not recognised, supported methods are")
  expect_error(analyse_baSAR(CWOSL.sub, verbose = FALSE,
                             source_doserate = c(0.04, 0.001)),
               'argument "signal.integral" is missing, with no default')
  expect_error(analyse_baSAR(CWOSL.sub, verbose = FALSE,
                             source_doserate = c(0.04, 0.001),
                             signal.integral = c(1:2)),
               'argument "background.integral" is missing, with no default')

  ## XLS_file
  expect_error(analyse_baSAR(CWOSL.sub, verbose = FALSE,
                             source_doserate = c(0.04, 0.001),
                             signal.integral = c(1:2),
                             background.integral = c(80:100),
                             XLS_file = list()),
               "Input type for 'XLS_file' not supported")
  expect_error(analyse_baSAR(CWOSL.sub, verbose = FALSE,
                             source_doserate = c(0.04, 0.001),
                             signal.integral = c(1:2),
                             background.integral = c(80:100),
                             XLS_file = "error"),
               "XLS_file does not exist")
  expect_error(analyse_baSAR(CWOSL.sub, verbose = FALSE,
                             source_doserate = c(0.04, 0.001),
                             signal.integral = c(1:2),
                             background.integral = c(80:100),
                             XLS_file = system.file("extdata/clippy.xls",
                                                    package = "readxl")),
               "The XLS_file requires at least 3 columns")
  SW({
  expect_error(analyse_baSAR(CWOSL.sub, verbose = FALSE,
                             source_doserate = c(0.04, 0.001),
                             signal.integral = c(1:2),
                             background.integral = c(80:100),
                             XLS_file = system.file("extdata/deaths.xls",
                                                    package = "readxl")),
               "One of the first 3 columns in your XLS_file has no header")
  })
  expect_error(analyse_baSAR(CWOSL.sub, verbose = FALSE,
                             source_doserate = c(0.04, 0.001),
                             signal.integral = c(1:2),
                             background.integral = c(80:100),
                             XLS_file = data.frame(a = NA, b = NA)),
               "The data.frame provided via 'XLS_file' must have at least 3")
  expect_warning(expect_error(
                 analyse_baSAR(CWOSL.sub, verbose = FALSE,
                               source_doserate = c(0.04, 0.001),
                               signal.integral = c(1:2),
                               background.integral = c(80:100),
                               XLS_file = data.frame(a = "error", b = 1, c = 2)),
                 "BIN-file names in XLS_file do not match the loaded BIN-files",
                 fixed = TRUE),
  "'error' not recognised or not loaded, skipped")

  expect_error(analyse_baSAR(CWOSL.sub, verbose = FALSE,
                             source_doserate = c(0.04, 0.001),
                             signal.integral = c(1:2),
                             background.integral = c(80:100),
                             XLS_file = data.frame(a = NA, b = 1, c = 2)),
               "Number of discs/grains = 0")

  expect_error(suppressWarnings(
      analyse_baSAR(Risoe.BINfileData2RLum.Analysis(CWOSL.sub),
                    verbose = FALSE)),
      "No records of the appropriate type were found")

  expect_warning(expect_output(
      analyse_baSAR(CWOSL.sub, verbose = FALSE,
                    source_doserate = c(0.04, 0.001),
                    signal.integral = c(1:2),
                    background.integral = c(80:100),
                    fit.method = "LIN", fit.force_through_origin = FALSE,
                    distribution = "error"),
      "[analyse_baSAR()] No pre-defined model for the requested distribution",
      fixed = TRUE),
      "Only multiple grain data provided, automatic selection skipped")

  expect_warning(expect_output(
      analyse_baSAR(CWOSL.sub, verbose = FALSE,
                    source_doserate = c(0.04, 0.001),
                    signal.integral = c(1:2),
                    background.integral = c(80:100),
                    distribution = "user_defined"),
      "[analyse_baSAR()] You specified a 'user_defined' distribution",
      fixed = TRUE),
      "Only multiple grain data provided, automatic selection skipped")

  expect_message(expect_output(suppressWarnings(
      analyse_baSAR(CWOSL.sub, verbose = FALSE,
                    source_doserate = c(0.04, 0.001),
                    signal.integral = c(1:2),
                    background.integral = c(80:100),
                    distribution = "cauchy",
                    baSAR_model = "error")),
      "Error parsing model file"),
      "'baSAR_model' provided, setting distribution to 'user_defined'")

  expect_error(suppressWarnings(
      analyse_baSAR(CWOSL.SAR.Data, verbose = FALSE,
                    source_doserate = c(0.04, 0.001),
                    signal.integral = c(1:2),
                    background.integral = c(80:100),
                    distribution = "user_defined")),
      "Channel numbers of Lx and Tx data differ")

  data(ExampleData.RLum.Analysis, envir = environment())
  expect_error(analyse_baSAR(list(IRSAR.RF.Data), verbose = FALSE),
               "At least two aliquots are needed for the calculation")
})

test_that("Full check of analyse_baSAR function", {
  skip_on_cran()

    set.seed(1)

    ##(3) run analysis
    ##please not that the here selected parameters are
    ##chosen for performance, not for reliability
    SW({
    results <- suppressWarnings(analyse_baSAR(
      object = CWOSL.sub,
      source_doserate = c(0.04, 0.001),
      signal.integral = c(1:2),
      background.integral = c(80:100),
      fit.method = "EXP",
      method_control = list(inits = list(
        list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1),
        list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 2),
        list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 3)
      )),
      plot = TRUE,
      verbose = TRUE,
      n.MCMC = 100,
      txtProgressBar = TRUE
    ))
    })

    expect_s4_class(results, class = "RLum.Results")
    expect_s3_class(results$summary, "data.frame")
    expect_s3_class(results$mcmc, "mcmc.list")
    expect_type(results$models, "list")
    expect_type(round(sum(results$summary[, c(6:9)]), 2),type = "double")

  ## rerun with previous results as input
  SW({
  results2 <- analyse_baSAR(
      object = results,
      plot = FALSE,
      verbose = FALSE,
      txtProgressBar = FALSE,
      n.MCMC = 100)

  expect_warning(analyse_baSAR(
      object = results,
      plot = FALSE,
      verbose = FALSE,
      txtProgressBar = FALSE,
      method_control = list(upper_centralD = 200),
      n.MCMC = 100),
      "You have modified the upper central_D boundary")

    expect_warning(analyse_baSAR(
      object = results,
      plot = FALSE,
      verbose = FALSE,
      txtProgressBar = FALSE,
      method_control = list(lower_centralD = 0),
      n.MCMC = 100),
      "You have modified the lower central_D boundary")

  suppressWarnings(expect_warning(analyse_baSAR(
      object = results,
      plot = FALSE,
      verbose = FALSE,
      txtProgressBar = FALSE,
      method_control = list(upper_centralD = 2),
      n.MCMC = 100),
      "Your lower_centralD and/or upper_centralD values seem not to fit",
      fixed = TRUE))

  analyse_baSAR(
      object = results,
      plot = TRUE,
      verbose = TRUE,
      txtProgressBar = FALSE,
      fit.method = "EXP+LIN",
      fit.includingRepeatedRegPoints = FALSE,
      fit.force_through_origin = FALSE,
      distribution = "log_normal",
      aliquot_range = 1:3,
      distribution_plot = "abanico",
      method_control = list(n.chains = 2, thin = 25),
      n.MCMC = 100)

  expect_warning(expect_error(
      analyse_baSAR(
          object = results,
          plot = TRUE,
          verbose = TRUE,
          txtProgressBar = FALSE,
          source_doserate = 2,
          fit.method = "LIN",
          distribution = "log_normal",
          method_control = list(n.chains = 2, variable.names = "Q"),
          n.MCMC = 100),
      "Plots for 'central_D' and 'sigma_D' could not be produced",
      fixed = TRUE),
      "'source_doserate' is ignored in this mode as it was already set")
  })

  results2@originator <- "unknown"
  expect_error(analyse_baSAR(object = results2),
               "'object' is of type 'RLum.Results', but was not produced by")
})

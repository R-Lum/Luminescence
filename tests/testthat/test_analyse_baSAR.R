## load data
data(ExampleData.BINfileData, envir = environment())
CWOSL.sub <- subset(CWOSL.SAR.Data,
                    subset = POSITION %in% c(1:3) & LTYPE == "OSL")

test_that("input validation", {
  skip_on_cran()

  expect_error(analyse_baSAR("error", verbose = FALSE),
               "File '.*error' does not exist") # windows CI needs the regexp
  expect_error(analyse_baSAR(list("error"), verbose = FALSE),
               "File '.*error' does not exist") # windows CI needs the regexp
  expect_error(analyse_baSAR(data.frame(), verbose = FALSE),
               "'object' should be of class 'Risoe.BINfileData', 'RLum.Results'")
  expect_error(analyse_baSAR(character(0), verbose = FALSE),
               "'object' cannot be an empty character")
  expect_error(analyse_baSAR(list(data.frame()), verbose = FALSE),
               "All elements of 'object' should be of class 'Risoe.BINfileData'")
  expect_error(analyse_baSAR(list(CWOSL.sub, "error"), verbose = FALSE),
               "'object' only accepts a list of objects of the same type")
  expect_error(analyse_baSAR(CWOSL.sub, n.MCMC = NULL),
               "'n.MCMC' should be a positive integer scalar")
  expect_error(analyse_baSAR(CWOSL.sub, verbose = FALSE),
               "'source_doserate' is missing, but the current implementation")
  expect_error(analyse_baSAR(CWOSL.sub, fit.method = "error"),
               "'fit.method' should be one of 'EXP', 'EXP+LIN' or 'LIN'",
               fixed = TRUE)
  expect_error(analyse_baSAR(CWOSL.sub, verbose = FALSE,
                             source_doserate = c(0.04, 0.001)),
               'argument "signal.integral" is missing, with no default')
  expect_error(analyse_baSAR(CWOSL.sub, verbose = FALSE,
                             source_doserate = c(0.04, 0.001),
                             signal.integral = c(1:2)),
               'argument "background.integral" is missing, with no default')

  ## CSV_file
  csv.file <- tempfile()
  expect_error(analyse_baSAR(CWOSL.sub, verbose = FALSE,
                             source_doserate = c(0.04, 0.001),
                             signal.integral = c(1:2),
                             background.integral = c(80:100),
                             CSV_file = list()),
               "'CSV_file' should be of class 'data.frame', 'character' or NULL")
  expect_error(analyse_baSAR(CWOSL.sub, verbose = FALSE,
                             source_doserate = c(0.04, 0.001),
                             signal.integral = c(1:2),
                             background.integral = c(80:100),
                             CSV_file = "error"),
               "'CSV_file' does not exist")
  data.table::fwrite(data.frame(BIN_file = "a", DISC = 1), file = csv.file)
  expect_error(analyse_baSAR(CWOSL.sub, verbose = FALSE,
                             source_doserate = c(0.04, 0.001),
                             signal.integral = c(1:2),
                             background.integral = c(80:100),
                             CSV_file = csv.file),
               "'CSV_file' should have at least 3 columns for the name of the")
  data.table::fwrite(data.frame(a = "error", b = 1, c = 2), file = csv.file)
  expect_error(analyse_baSAR(CWOSL.sub, verbose = FALSE,
                             source_doserate = c(0.04, 0.001),
                             signal.integral = c(1:2),
                             background.integral = c(80:100),
                             CSV_file = csv.file),
               "One of the first 3 columns in 'CSV_file' has no header")
  expect_error(analyse_baSAR(CWOSL.sub, verbose = FALSE,
                             source_doserate = c(0.04, 0.001),
                             signal.integral = c(1:2),
                             background.integral = c(80:100),
                             CSV_file = data.frame(a = NA, b = NA)),
               "'CSV_file' should have at least 3 columns for the name of the")
  expect_warning(expect_error(
                 analyse_baSAR(CWOSL.sub, verbose = FALSE,
                               sigmab = list(0.23), sig0 = list(0.02),
                               source_doserate = list(0.04, 0.001),
                               signal.integral = list(1, 2),
                               signal.integral.Tx = c(2:4),
                               background.integral = list(80, 100),
                               background.integral.Tx = c(80:100),
                               CSV_file = data.frame(a = "error", b = 1, c = 2)),
                 "BIN-file names provided via 'CSV_file' do not match the loaded BIN-files",
                 fixed = TRUE),
  "'error' not recognised or not loaded, skipped")

  expect_error(analyse_baSAR(CWOSL.sub, verbose = FALSE,
                             source_doserate = c(0.04, 0.001),
                             signal.integral = c(1:2),
                             background.integral = c(80:100),
                             CSV_file = data.frame(a = NA, b = 1, c = 2)),
               "Number of discs/grains = 0")

  SW({
  obj <- Risoe.BINfileData2RLum.Analysis(CWOSL.sub)
  expect_error(suppressWarnings(
      analyse_baSAR(obj, verbose = TRUE)),
      "No records of the appropriate type were found")
  expect_error(suppressWarnings(
    analyse_baSAR(obj, recordType = "NONE", verbose = TRUE)),
    "No records of the appropriate type were found")
  })

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

  SW({
  data(ExampleData.RLum.Analysis, envir = environment())
  expect_error(analyse_baSAR(list(IRSAR.RF.Data), verbose = TRUE),
               "At least two aliquots are needed for the calculation")

  expect_warning(expect_output(
      analyse_baSAR(list(CWOSL.sub, CWOSL.sub), verbose = TRUE,
                    source_doserate = c(0.04, 0.001),
                    signal.integral = c(1:2),
                    background.integral = c(80:100),
                    fit.method = "LIN", fit.force_through_origin = FALSE,
                    distribution = "normal",
                    n.MCMC = 75)),
      "'1' is a duplicate and therefore removed from the input")

  CWOSL.min <- subset(CWOSL.sub, subset = ID < 20)
  expect_warning(expect_error(
      analyse_baSAR(CWOSL.min, source_doserate = c(0.04, 0.001),
                    signal.integral = c(1:2),
                    background.integral = c(5:15),
                    method_control = list(n.chains = 1),
                    n.MCMC = 10),
      "In input 1 the number of data points (19) is not a multiple of the",
      fixed = TRUE),
      "Only multiple grain data provided, automatic selection skipped")
  })
})

test_that("Full check of analyse_baSAR function", {
  skip_on_cran()

    set.seed(1)
    ##(3) run analysis
    ##please not that the here selected parameters are
    ##chosen for performance, not for reliability
    SW({
    expect_snapshot_RLum(results <- suppressWarnings(analyse_baSAR(
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
      )),
      tolerance = 1.5e-6)
    })

    ## source dose rate only scalar
    SW({
      expect_snapshot_RLum(results <- suppressWarnings(analyse_baSAR(
        object = CWOSL.sub,
        source_doserate = c(0.04),
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
      )),
      tolerance = 1.5e-6)
    })

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

  expect_warning(
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
      plot.single = TRUE,
      n.MCMC = 100),
    "'plot.single' is deprecated, use 'plot_singlePanels' instead")

  expect_message(
      analyse_baSAR(
          object = results,
          plot = FALSE,
          verbose = TRUE,
          txtProgressBar = FALSE,
          fit.method = "EXP",
          fit.force_through_origin = TRUE,
          distribution = "cauchy",
          aliquot_range = 100:300,
          distribution_plot = NULL,
          method_control = list(n.chains = 1, thin = 25),
          n.MCMC = 100),
      "Error: 'aliquot_range' out of bounds, input ignored")

  expect_warning(expect_message(
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

  CWOSL.mod <- CWOSL.sub
  CWOSL.mod@METADATA$SEL <- FALSE
  expect_warning(expect_null(analyse_baSAR(CWOSL.mod, verbose = TRUE,
                                           source_doserate = c(0.04, 0.001),
                                           signal.integral = c(1:2),
                                           background.integral = c(80:100),
                                           method_control = list(n.chains = 1),
                                           n.MCMC = 100)),
                 "No records selected, NULL returned")

  CWOSL.mod <- CWOSL.sub
  CWOSL.mod@METADATA$SEL[19:24] <- FALSE
  expect_message(analyse_baSAR(CWOSL.mod, verbose = TRUE,
                               source_doserate = c(0.04, 0.001),
                               signal.integral = c(1:2),
                               background.integral = c(80:100),
                               method_control = list(n.chains = 1),
                               n.MCMC = 100),
                 "Record pre-selection in BIN-file detected")

  CWOSL.mod <- CWOSL.sub
  CWOSL.mod@METADATA$GRAIN[-c(19:24)] <- 2
  expect_warning(analyse_baSAR(CWOSL.mod, verbose = TRUE,
                               source_doserate = c(0.04, 0.001),
                               signal.integral = c(1:2),
                               background.integral = c(80:100),
                               method_control = list(n.chains = 1),
                               n.MCMC = 100),
                 "Automatic grain selection: 3 curves with grain index 0 have been removed")
  })

  results2@originator <- "unknown"
  expect_error(analyse_baSAR(object = results2),
               "'object' is of type 'RLum.Results', but was not produced by")

  results2@originator <- "analyse_baSAR"
  results2@data$input_object <- results2$input_object[1:2, ]
  expect_message(expect_null(analyse_baSAR(object = results2)),
                 "Error: Number of aliquots < 3, NULL returned")

  SW({
  expect_warning(analyse_baSAR(CWOSL.sub, source_doserate = c(0.04, 0.001),
                               signal.integral = c(1:2),
                               background.integral = c(8:10),
                               method_control = list(n.chains = 1),
                               n.MCMC = 10),
                 "Number of background channels for Tx < 25")

  analyse_baSAR(CWOSL.sub,
                CSV_file = CWOSL.sub@METADATA[, c("FNAME", "POSITION", "GRAIN")],
                source_doserate = c(0.04, 0.001),
                signal.integral = c(1:2),
                background.integral = c(8:10),
                method_control = list(n.chains = 1),
                aliquot_range = 1:2,
                n.MCMC = 10)

  vnames <- c("central_D", "sigma_D", "")
  expect_message(analyse_baSAR(CWOSL.sub,
                               source_doserate = c(0.04, 0.001),
                               signal.integral = 1:2,
                               background.integral = 60:100,
                               method_control = list(n.chains = 1,
                                                     variable.names = vnames),
                               plot_reduced = FALSE,
                               n.MCMC = 10, verbose = FALSE),
                 "Dose-response curves could not be plotted as 'variable.names'")

  vnames <- c("D", "Q", "a", "b", "c", "g")
  expect_message(analyse_baSAR(CWOSL.sub,
                               source_doserate = c(0.04, 0.001),
                               signal.integral = 1:2,
                               background.integral = 60:100,
                               method_control = list(n.chains = 1,
                                                     variable.names = vnames),
                               n.MCMC = 10, verbose = FALSE),
                 "Plots for 'central_D' and 'sigma_D' could not be produced")
  })
})

test_that("regression tests", {
  skip_on_cran()

  ## issue 407
  SW({
  expect_warning(expect_s4_class(
      analyse_baSAR(CWOSL.sub, verbose = FALSE, plot = FALSE,
                    source_doserate = c(0.04, 0.001),
                    signal.integral = c(1:2),
                    background.integral = c(80:100),
                    method_control = list(n.chains = 1, thin = 60),
                    n.MCMC = 60),
      "RLum.Results"),
      "'thin = 60' is too high for 'n.MCMC = 60', reset to 30")
  })

  ## check parameters irradiation times
  SW({
  expect_s4_class(suppressWarnings(analyse_baSAR(CWOSL.sub,
                verbose = FALSE,
                plot = FALSE,
                source_doserate = c(0.04, 0.001),
                signal.integral = c(1:2),
                irradiation_times = c(0, 0, 0, 0, 0, 0, 450, 450, 450, 0, 0, 0,
                                      1050, 1050, 1050, 0, 0, 0, 2000, 2000, 2000, 0, 0, 0,
                                      2550, 2550, 2550, 0, 0, 0, 450, 450,
                                      450, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                background.integral = c(80:100),
                n.MCMC = 10)), "RLum.Results")
  })

  ## have all irradiation times identical
  expect_message(
    suppressWarnings(analyse_baSAR(CWOSL.sub,
                verbose = FALSE,
                plot = FALSE,
                source_doserate = c(0.04, 0.001),
                signal.integral = c(1:2),
                irradiation_times = c(0),
                background.integral = c(80:100),
                n.MCMC = 10)),
     regexp = "Error: All irradiation times are identical, NULL returned")

  ## test removal of unwanted curves
  ## check irradiation times assignment and non-OSL curve removal
  tmp_object <- Risoe.BINfileData2RLum.Analysis(CWOSL.sub)
  tmp_curves <- tmp_object[[3]]@records[[1]]
  tmp_curves@recordType <- "NONE"
  tmp_object[[3]]@records <- c(tmp_object[[3]]@records, rep(tmp_curves,2))

  SW({
  expect_s4_class(
    suppressWarnings(
      analyse_baSAR(tmp_object,
                verbose = TRUE,
                plot = FALSE,
                recordType = c("OSL", "NONE"),
                source_doserate = c(0.04, 0.001),
                signal.integral = c(1:2),
                background.integral = c(80:100),
                n.MCMC = 10)), "RLum.Results")
  })

  tmp_object[[3]] <- set_RLum("RLum.Analysis")
  SW({
  expect_message(expect_null(
      analyse_baSAR(tmp_object,
                    verbose = FALSE,
                    recordType = c("OSL", "NONE"),
                    source_doserate = c(0.04, 0.001),
                    signal.integral = c(1:2),
                    background.integral = c(80:100),
                    n.MCMC = 10)),
      "Object conversion failed, NULL returned")
  })
})

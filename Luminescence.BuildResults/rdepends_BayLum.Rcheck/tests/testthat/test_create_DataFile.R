test_that("Full function test", {
  testthat::skip_on_cran()
  local_edition(3)

# Prepare test ------------------------------------------------------------
  ## set path to files configuration and BIN/BINX files
  yaml_file <- system.file("extdata/example.yml", package = "BayLum")

  ## manually import yaml file
  config_file <- yaml::read_yaml(yaml_file)

  ## BIN/BINX files
  samp1_file <- system.file("extdata/samp1/bin.bin", package = "BayLum")
  samp2_file <- system.file("extdata/samp2/bin.bin", package = "BayLum")

  ## import BIN files
  samp1 <<- Luminescence::read_BIN2R(samp1_file, verbose = FALSE) |>
    subset(POSITION == 2 & GRAIN == 32)
  samp2 <<- Luminescence::read_BIN2R(samp2_file, verbose = FALSE) |>
    subset(POSITION == 2 & GRAIN == 32)

# Plain run ---------------------------------------------------------------
  ## check verbose mode
  out <- expect_type(create_DataFile(yaml_file), "list")

  ## check verbose off mode
  out <- expect_type(create_DataFile(yaml_file, verbose = FALSE), "list")

  ## check import with YAML as list
  out <- expect_type(create_DataFile(config_file, verbose = FALSE), "list")

  ## check with real import from file
  config_with_file <- config_file

    ## single file
    config_with_file[[2]]$files <- samp2_file
    out <- expect_type(create_DataFile(config_with_file, verbose = FALSE), "list")

    ## two files
    config_with_file[[2]]$files <- c(samp2_file, samp2_file)
    out <- expect_type(create_DataFile(config_with_file, verbose = FALSE), "list")
    expect_equal(nrow(out$LT[[2]]), 2)

  ## provide dose points for at least one
  config_dose_points <- config_file
  config_dose_points[[2]]$settings$dose_points <-  c(15, 30, 60, 100, 0, 15)
  expect_type(create_DataFile(config_dose_points, verbose = FALSE), "list")

    ## variation, provide more dose points
    config_dose_points[[2]]$settings$dose_points <-  c(15, 30, 60, 100, 0, 15, 15)
    expect_type(create_DataFile(config_dose_points, verbose = FALSE), "list")

  ## check attribute
  expect_true(attributes(out)$originator == "create_DataFile")

  ## add this test ... it does not work yet ... check
  # AgeS_Computation(
  #   DATA = out,
  #   Nb_sample = 2,
  #   SampleNames = c("samp1", "samp2")
  #   )

# Try to trigger various errors -------------------------------------------

  ## test YAML file consistency ----
    ### top level -------
    config_err <- config_file
    names(config_err[[2]]) <- c("error", "error", "error")
    expect_error(
      object = create_DataFile(config_file = config_err, verbose = FALSE),
      regexp = "\\[create\\_DataFile\\(\\)\\] Missing or.+")

    ### settings level --------
    config_err <- config_file
    names(config_err[[2]]$settings) <- c("error", "error", "error")
    expect_error(
      object = create_DataFile(config_file = config_err, verbose = FALSE),
      regexp = "\\[create\\_DataFile\\(\\)\\] Missing or.+")

    ## not enough dose points
    config_dose_points[[2]]$settings$dose_points <-  c(15, 30, 60)
    expect_error(
      object = create_DataFile(config_dose_points, verbose = FALSE),
      regexp = "\\[create\\_DataFile\\(\\)\\] Not enough regeneration dose points specified.+")

    ### rules level -------
    config_err <- config_file
    names(config_err[[2]]$settings$rules) <- c("error", "error", "error")
    expect_error(
      object = create_DataFile(config_file = config_err, verbose = FALSE),
      regexp = "\\[create\\_DataFile\\(\\)\\] Missing or.+")

  ## import tests ----
    ## rm object from working environment
    samp_2 <- samp2
    rm(samp2, envir = as.environment(1))
    expect_error(
      object = create_DataFile(config_file, verbose = FALSE),
      regexp = "\\[create\\_DataFile\\(\\)\\] <samp2> is not.+")
    samp2 <<- samp_2

    ## one object is not of type RLum.Analysis
    samp_2 <- samp2
    samp2 <<- "error"
    expect_error(
      object = create_DataFile(config_file, verbose = FALSE),
      regexp = "\\[create\\_DataFile\\(\\)\\] Wrong object type. All.+")
    samp2 <<- samp_2

    ## non-unique sample names
    config_file_err <- config_file
    config_file_err[[2]]$sample <- "samp1"
    expect_error(
      object = create_DataFile(config_file_err, verbose = FALSE),
      regexp = "\\[create\\_DataFile\\(\\)\\] Sample names must be unique.+")

    ## modify channel numbers in dataset
    tmp <- samp_bck <- samp2
    tmp@DATA[[1]] <- tmp@DATA[[1]][1:10]
    tmp@METADATA$NPOINTS[1] <- 10
    samp2 <<- tmp
    expect_error(
      object = create_DataFile(config_file, verbose = FALSE),
      regexp = "\\[create\\_DataFile\\(\\)\\] All curves within one sample.+")
    samp2 <<- samp_bck
    rm(tmp)

    ## check integration limits
    config_file_err <- config_file
    config_file_err[[1]]$settings$rules$endSignal <- 1000
    expect_error(
      object = create_DataFile(config_file_err, verbose = FALSE),
      regexp = "\\[create\\_DataFile\\(\\)\\] Check your integration limit settings.+")

    ## less than four records?
    samp_bck <- samp2
    tmp <- Luminescence::Risoe.BINfileData2RLum.Analysis(samp_bck)
    tmp <- Luminescence::get_RLum(tmp, record.id = c(1:3), drop = FALSE)
    samp2 <<- tmp
    expect_error(
      object = create_DataFile(config_file, verbose = FALSE),
      regexp = "\\[create\\_DataFile\\(\\)\\] All records must have at least.+")
    samp2 <<- samp_bck
    rm(tmp)

    ## make sure all records have the same number of curves
    ## this goes only for two records
    samp_bck <- samp1
    tmp <- Luminescence::Risoe.BINfileData2RLum.Analysis(samp1)
    tmp <- list(tmp, tmp)
    tmp[[2]]@records <- tmp[[2]]@records[1:6]
    samp1 <<- tmp
    expect_error(
      object = create_DataFile(config_file, verbose = FALSE),
      regexp = "\\[create\\_DataFile\\(\\)\\] For one sample all records must have the same number.+")
    samp1 <<- samp_bck
    rm(tmp)

    ## curves must be a multiple of two
    samp_bck <- samp2
    tmp <- Luminescence::Risoe.BINfileData2RLum.Analysis(samp_bck)
    tmp <- Luminescence::get_RLum(tmp, record.id = c(1:7), drop = FALSE)
    samp2 <<- tmp
    expect_error(
      object = create_DataFile(config_file, verbose = FALSE),
      regexp = "\\[create\\_DataFile\\(\\)\\] Curves must be multiple of two.+")
    samp2 <<- samp_bck
    rm(tmp)

    ## check warning for empty records
    samp_bck <- samp2
    tmp <- Luminescence::Risoe.BINfileData2RLum.Analysis(samp_bck)
    tmp <- list(tmp, Luminescence::set_RLum("RLum.Analysis"))
    samp2 <<- tmp
    expect_warning(
      object = create_DataFile(config_file, verbose = FALSE),
      regexp = "\\[create\\_DataFile\\(\\)\\] Empty RLum.Analysis records.+")
    samp2 <<- samp_bck
    rm(tmp)

  ## clear environment (just in case)
  rm(samp1, envir = as.environment(1))
  rm(samp2, envir = as.environment(1))

})


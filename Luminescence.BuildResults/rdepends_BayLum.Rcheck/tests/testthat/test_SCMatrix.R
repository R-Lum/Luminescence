test_that("Interactive tests", {
  testthat::skip_on_cran()
  local_edition(3)

  ## set y
  f_OK <- file()
  write("y", f_OK)

  ## set wrong answer
  f_stop <- file()
  write("stop", f_stop)

  ## simple test, DATA not given
  ## run interactive test
  options("SCMatrix.con" = f_OK) # set connection option
  t <- SCMatrix(
     Nb_sample = 2,
     SampleNames = c("sample1","sample2"))

  close(f_OK)

  ## check the results
  expect_type(t, type = "double")

  ## simple test, DATA not given
  ## run interactive test
  options("SCMatrix.con" = f_stop) # set connection option
  expect_error(SCMatrix(
    Nb_sample = 2,
    SampleNames = c("sample1","sample2")))

  close(f_stop)

  ## run DATA
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

  DATA <- expect_type(create_DataFile(yaml_file), "list")


  ## run interactive test
  f_OK <- file()
  write("y", f_OK)

  options("SCMatrix.con" = f_OK) # set connection option
  t <- SCMatrix(
    DATA = DATA)

  close(f_OK)

  ## check the results
  expect_type(t, type = "double")

})



test_that("Interactive tests", {
  testthat::skip_on_cran()
  local_edition(3)

  ## simple test --------
  t <- expect_type(SC_Ordered(3), type = "double")
  expect_equal(nrow(t), expected = 4)

  ## run test with DATA ----------------
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
  t <- expect_type(SC_Ordered(DATA), type = "double")
  expect_equal(nrow(t), 3)


})



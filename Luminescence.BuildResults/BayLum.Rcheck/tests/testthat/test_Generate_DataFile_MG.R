test_that("Full function test", {
  testthat::skip_on_cran()
  local_edition(3)

  ## prepare folder
  path <- system.file("extdata/FER1", "", package = "BayLum")
  folder <- ""
  nbsample <- 1

  ## run function with default settings
  DATA <- expect_silent(Generate_DataFile_MG(
    Path = path,
    FolderNames = folder,
    Nb_binfile = 1,
    Nb_sample = nbsample,
    verbose = FALSE))

  expect_type(DATA, "list")
  expect_length(DATA, 9)

  ## run verbose
  expect_output(Generate_DataFile_MG(
    Path = path,
    FolderNames = folder,
    Nb_sample = nbsample,
    verbose = TRUE))

  ## run with force_run1_at_a_time = TRUE
  DATA <- expect_silent(Generate_DataFile_MG(
    Path = path,
    FolderNames = folder,
    Nb_sample = nbsample,
    force_run1_at_a_time = TRUE,
    verbose = FALSE))

  ## trigger BIN-file warning
  expect_warning(Generate_DataFile_MG(
    Path = path,
    Nb_binfile = 2,
    FolderNames = folder,
    Nb_sample = nbsample,
    force_run1_at_a_time = TRUE,
    verbose = FALSE), regexp = "\\[Generate\\_DataFile\\_MG\\(\\)\\] The total number.*")
})


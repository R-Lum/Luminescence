test_that("Full function test", {
  testthat::skip_on_cran()
  local_edition(3)

  ## set tempdir
  tempdir <- tempdir()

  # Crash function ----------------------------------------------------------
  expect_error(create_FolderTemplates(), "Input for 'path' is missing")
  expect_error(create_FolderTemplates(paste0(tempdir,"/Desktop/Test_Micky")), "does not exist")
  expect_error(create_FolderTemplates(tempdir, mode = "error"), "mode = 'error' not supported")

  # Run function ------------------------------------------------------------
  expect_message(create_FolderTemplates(path = tempdir, n_folders = 1), "All templates created. Please modify the parameters according to your data")


})

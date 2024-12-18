test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(import_Data(data.frame()),
               "'file' should be of class 'character' or 'list'")
  expect_message(import_Data(system.file("extdata/QNL84_2_bleached.txt")),
                 "Unknown file format, nothing imported")
})

test_that("Test general import", {
  testthat::skip_on_cran()

  ## BINX
  SW({
  expect_type(
    object = import_Data(system.file("extdata/BINfile_V8.binx", package = "Luminescence")),
    type = "list")
  })

  ## XSYG
  expect_type(
    object = import_Data(system.file("extdata/XSYG_file.xsyg", package = "Luminescence")),
    type = "list")

  ## PSL
  expect_s4_class(
    object = import_Data(system.file("extdata/DorNie_0016.psl", package = "Luminescence"))[[1]],
    class = "RLum.Analysis")

  ## DAT (Daybreak)
  expect_type(
    object = import_Data(system.file("extdata/Daybreak_TestFile.DAT", package = "Luminescence")),
    type = "list")

  ## TXT (Daybreak)
  expect_type(
    object = import_Data(system.file("extdata/Daybreak_TestFile.txt", package = "Luminescence")),
    type = "list")

  ## RF
  expect_type(
    object = import_Data(system.file("extdata/RF_file.rf", package = "Luminescence")),
    type = "list")

  ## TIFF
  expect_s4_class(
    object = import_Data(system.file("extdata/TIFFfile.tif", package = "Luminescence"))[[1]],
    class = "RLum.Data.Image")

  ## OSL
  expect_s4_class(
    object = import_Data(system.file("extdata/HeliosOSL_Example.osl", package = "Luminescence"))[[1]],
    class = "RLum.Analysis")

  ## use character and list as input
  files <- c(
    system.file("extdata/HeliosOSL_Example.osl", package = "Luminescence"),
    system.file("extdata/HeliosOSL_Example.osl", package = "Luminescence")
  )
  expect_type(
    object = import_Data(files, verbose = FALSE),
    type = "list")
  expect_type(
    object = import_Data(as.list(files), verbose = FALSE),
    type = "list")

  ## mix input
  files <- c(
    system.file("extdata/BINfile_V8.binx", package = "Luminescence"),
    system.file("extdata/HeliosOSL_Example.osl", package = "Luminescence"),
    system.file("extdata/RF_file.rf", package = "Luminescence"),
    system.file("extdata/HeliosOSL_Example.osl", package = "Luminescence"),
    system.file("extdata/TIFFfile.tif", package = "Luminescence"),
    system.file("extdata/XSYG_file.xsyg", package = "Luminescence"))
  SW({
  t <- expect_type(
    object = import_Data(files, verbose = FALSE),
    type = "list")
  })
  expect_equal(length(t), 16)

})

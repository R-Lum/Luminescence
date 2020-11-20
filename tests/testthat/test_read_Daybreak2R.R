test_that("Test functionality", {
  testthat::skip_on_cran()
  local_edition(3)

  ##TXT
  ##basic import options
  expect_type(read_Daybreak2R(
    file = system.file("extdata/Daybreak_TestFile.txt", package = "Luminescence")
  ), "list")

  ##verbose off
  expect_type(read_Daybreak2R(
    file = system.file("extdata/Daybreak_TestFile.txt", package = "Luminescence"),
    verbose = FALSE
  ), "list")

  ##txtProgressbar off
  expect_type(read_Daybreak2R(
    file = system.file("extdata/Daybreak_TestFile.txt", package = "Luminescence"),
    txtProgressBar = FALSE
  ), "list")

  ##DAT
  ##basic import options
  expect_type(read_Daybreak2R(
    file = system.file("extdata/Daybreak_TestFile.DAT", package = "Luminescence")
  ), "list")

  ##test silence
  expect_silent(read_Daybreak2R(
    file = system.file("extdata/Daybreak_TestFile.DAT", package = "Luminescence"), verbose = FALSE))

})

context("test_read_Daybreak2R")

test_that("Test functionality", {
  testthat::skip_on_cran()

  ##TXT
  ##basic import options
  expect_is(read_Daybreak2R(
    file = system.file("extdata/Daybreak_TestFile.txt", package = "Luminescence")
  ), "list")

  ##verbose off
  expect_is(read_Daybreak2R(
    file = system.file("extdata/Daybreak_TestFile.txt", package = "Luminescence"),
    verbose = FALSE
  ), "list")

  ##txtProgressbar off
  expect_is(read_Daybreak2R(
    file = system.file("extdata/Daybreak_TestFile.txt", package = "Luminescence"),
    txtProgressBar = FALSE
  ), "list")

  ##DAT
  ##basic import options
  expect_is(read_Daybreak2R(
    file = system.file("extdata/Daybreak_TestFile.DAT", package = "Luminescence")
  ), "list")

})

test_that("test convert functions", {
  testthat::skip_on_cran()
  local_edition(3)

  ##test for errors
  expect_error(convert_BIN2CSV(file = "", export = FALSE),
               regexp = "[read_BIN2R()] File does not exist!",
               fixed = TRUE)
  expect_error(convert_Daybreak2CSV(file = "", export = FALSE),
               regexp = "[read_Daybreak2R()] file name does not seem to exist.",
               fixed = TRUE)
  #expect_error(convert_PSL2CSV(file = "", export = FALSE))
  expect_error(suppressWarnings(convert_XSYG2CSV(file = "", export = FALSE)))

  ##test conversion itself
    ##BIN2CSV
    data(ExampleData.BINfileData, envir = environment())
    expect_type(convert_BIN2CSV(subset(CWOSL.SAR.Data, POSITION == 1), export = FALSE), "list")

    ##XSYG2CSV
    data(ExampleData.XSYG, envir = environment())
    expect_type(convert_XSYG2CSV(OSL.SARMeasurement$Sequence.Object[1:10], export = FALSE), "list")

})

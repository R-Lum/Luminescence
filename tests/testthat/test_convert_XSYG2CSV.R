test_that("test convert functions", {
  testthat::skip_on_cran()

  ##test for errors
  expect_error(convert_XSYG2CSV(),
               "file is missing")
  expect_error(convert_BIN2CSV(),
               "file is missing")
  expect_error(convert_BIN2CSV(file = "", export = FALSE),
               regexp = "[read_BIN2R()] File does not exist!",
               fixed = TRUE)
  #expect_error(convert_PSL2CSV(file = "", export = FALSE))
  expect_error(suppressWarnings(convert_XSYG2CSV(file = "", export = FALSE)))

  ##test conversion itself
    ##BIN2CSV
    data(ExampleData.BINfileData, envir = environment())
    expect_type(convert_BIN2CSV(subset(CWOSL.SAR.Data, POSITION == 1), export = FALSE), "list")
    expect_null(convert_BIN2CSV(subset(CWOSL.SAR.Data, POSITION == 1),
                                export = TRUE, path = tempdir()))

    ##XSYG2CSV
    data(ExampleData.XSYG, envir = environment())
    expect_type(convert_XSYG2CSV(OSL.SARMeasurement$Sequence.Object[1:10],
                                 export = FALSE), "list")
    expect_null(convert_XSYG2CSV(OSL.SARMeasurement$Sequence.Object[1:10],
                                 export = TRUE, path = tempdir()))
})

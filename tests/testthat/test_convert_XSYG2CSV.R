test_that("input validation", {
  testthat::skip_on_cran()

  ##test for errors
  expect_error(convert_XSYG2CSV(),
               "'file' should be of class 'character' or 'RLum'")
  expect_error(convert_BIN2CSV(),
               "'file' should be of class 'character' or 'Risoe.BINfileData'")
  expect_error(convert_BIN2CSV(file = "error", export = FALSE),
               "File does not exist")
  expect_error(convert_PSL2CSV(file = "error"),
               "No .psl files found")
  expect_error(expect_message(convert_XSYG2CSV(file = "", export = FALSE),
                              "XML file not readable, nothing imported"),
               "'object' should be of class 'RLum.Analysis', 'RLum.Data.Curve'")
})

test_that("test convert functions", {
  testthat::skip_on_cran()

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

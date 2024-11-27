test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(convert_XSYG2CSV(),
               "'file' should be of class 'character' or 'RLum'")
  expect_error(convert_XSYG2CSV(character(0)),
               "'file' cannot be an empty character")
  expect_error(convert_PSL2CSV(file = "error"),
               "No .psl files found")
  SW({
  expect_error(expect_message(convert_XSYG2CSV(file = "", export = FALSE),
                              "XML file not readable, nothing imported"),
               "'object' should be of class 'RLum.Analysis', 'RLum.Data.Curve'")
  })
})

test_that("test convert functions", {
  testthat::skip_on_cran()

    data(ExampleData.XSYG, envir = environment())
    expect_type(convert_XSYG2CSV(OSL.SARMeasurement$Sequence.Object[1:10],
                                 export = FALSE), "list")
    expect_null(convert_XSYG2CSV(OSL.SARMeasurement$Sequence.Object[1:10],
                                 export = TRUE, path = tempdir()))
})

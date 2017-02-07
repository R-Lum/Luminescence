context("Test old Analyse_SAROSLdata()")

test_that("full example test", {
  testthat::skip_on_cran()
  data(ExampleData.BINfileData, envir = environment())
  output <- Analyse_SAR.OSLdata(input.data = CWOSL.SAR.Data,
                                signal.integral = c(1:5),
                                background.integral = c(900:1000),
                                position = c(1:1),
                                output.plot = FALSE)

  ##checks
  expect_is(output, "list")
  expect_length(output, 3)


})

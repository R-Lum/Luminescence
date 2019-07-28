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
  
  ## errors
  expect_error({ Analyse_SAR.OSLdata() }, 
               regexp = "No input data given")
  expect_error({ Analyse_SAR.OSLdata(input.data = CWOSL.SAR.Data) }, 
               regexp = "No signal integral is given")
  expect_error({ Analyse_SAR.OSLdata(input.data = CWOSL.SAR.Data, signal.integral = 1:3) }, 
               regexp = "No background integral is given")
  expect_error({ Analyse_SAR.OSLdata(input.data = subset(CWOSL.SAR.Data, LTYPE == "IRSL"), 
                                     signal.integral = 1:3,
                                     background.integral = 200:250) }, 
               regexp = "No 'OSL' curves found")
  
  ## should work
  expect_is({ Analyse_SAR.OSLdata(input.data = CWOSL.SAR.Data, signal.integral = 1:3,
                                  background.integral = 200:250, position = 1,
                                  background.count.distribution = "non-poisson",
                                  sigmab = 0.1, output.plot = TRUE) }, "list")
  expect_is({
    tmp <- subset(CWOSL.SAR.Data, LTYPE == "OSL" & POSITION == 1 & ID <= 457)
    Analyse_SAR.OSLdata(tmp, 1:3, 200:250, output.plot = TRUE, output.plot.single = TRUE)
  }, "list")
  
  
  
})

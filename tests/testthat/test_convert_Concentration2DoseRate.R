test_that("basic checks", {
  testthat::skip_on_cran()

  ## template
  SW({
  template <- expect_s3_class(convert_Concentration2DoseRate(), "data.frame")
  })

  ## break function
  expect_error(convert_Concentration2DoseRate(input = "fail"),
               "'input' should be of class 'data.frame' or 'matrix'")

  expect_error(convert_Concentration2DoseRate(input = data.frame(x = 1, y = 2)),
               "Number of rows/columns in input does not match the requirements")

  expect_error(convert_Concentration2DoseRate(
      suppressMessages(convert_Concentration2DoseRate()), conversion = "error"),
    "'conversion' should be one of 'Guerinetal2011', 'Cresswelletal2018'")

  template[[1]] <- "fail"
  expect_error(convert_Concentration2DoseRate(template),
               "As mineral only 'FS' or 'Q' is supported")

  ## run function
  ## for FS
  df <-
    data.frame(
      Mineral = "FS",
      K = 2.13,
      K_SE = 0.07,
      Th = 9.76,
      Th_SE = 0.32,
      U = 2.24,
      U_SE = 0.12,
      GrainSize = 200,
      WaterContent = 30,
      WaterContent_SE = 5
    )

   expect_s4_class(object = convert_Concentration2DoseRate(df), class = "RLum.Results")

   ## for Q
   df$Mineral <- "Q"
   expect_s4_class(object = convert_Concentration2DoseRate(df), class = "RLum.Results")

})

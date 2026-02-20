df <- data.frame(Mineral = "FS",
                 K = 2.13, K_SE = 0.07,
                 Th = 9.76, Th_SE = 0.32,
                 U = 2.24, U_SE = 0.12,
                 GrainSize = 200, WaterContent = 30, WaterContent_SE = 5)

test_that("input validation", {
  testthat::skip_on_cran()

  ## template
  SW({
  template <- expect_s3_class(convert_Concentration2DoseRate(), "data.frame")
  })

  expect_error(convert_Concentration2DoseRate("fail"),
               "'object' should be of class 'data.frame' or 'matrix'")
  expect_error(convert_Concentration2DoseRate(data.frame(x = 1, y = 2)),
               "'object' should have 10 columns")
  expect_error(convert_Concentration2DoseRate(rbind(df, df)),
               "'object' should have only one row")
  expect_error(convert_Concentration2DoseRate(template),
               "'object' should not contain NA values")
  expect_error(convert_Concentration2DoseRate(df, conversion = "error"),
               "'conversion' should be one of 'Guerinetal2011', 'Cresswelletal2018'")

  df[[8]] <- "error"
  expect_error(convert_Concentration2DoseRate(df),
               "Each element of 'object' other than the first should be of class")
  df[[8]] <- 9999
  expect_error(convert_Concentration2DoseRate(df),
               "No attenuation data available for the grain size provided")
  df[[1]] <- "fail"
  expect_error(convert_Concentration2DoseRate(df),
               "As mineral only 'FS' or 'Q' is supported")
})

test_that("deprecated argument", {
  testthat::skip_on_cran()

  ## deprecated argument
  expect_warning(convert_Concentration2DoseRate(input = df),
                 "'input' was deprecated in v1.2.0, use 'object' instead")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  df$Mineral <- "FS"
  expect_snapshot_RLum(convert_Concentration2DoseRate(df))

  df$Mineral <- "Q"
  expect_snapshot_RLum(convert_Concentration2DoseRate(df))
})

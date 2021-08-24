test_that("Full function test", {
  testthat::skip_on_cran()
  local_edition(3)

  ##sigma_s
  sigma_s <-  c(
    s_betaK = 0.010,
    s_betaU = 0.007,
    s_betaTh = 0.006,
    s_gammaK = 0.010,
    s_gammaU = 0.007,
    s_gammaTh = 0.006,
    s_gammaDR = 0.05,
    s_CAL = 0.020,
    s_intDR = 0.030)

  ##try to crash the function
  expect_error(create_ThetaMatrix(input = 2))
  expect_error(create_ThetaMatrix(input = "test"), regexp = "File test does not exist!")
  expect_error(create_ThetaMatrix(input = data.frame(), sigma_s = sigma_s), regexp = "The input data.frame needs at least 2 rows!")
  expect_error(create_ThetaMatrix(input = data.frame()), regexp =  "argument \"sigma_s\" is missing, with no default")
  expect_error(create_ThetaMatrix(input = data.frame(), sigma_s = c(test = 1)),
               regexp = "Value names do not match in 'sigma_s', please check the manual!")

  ##create reference dataset
  ##without anything
  expect_type(suppressMessages(create_ThetaMatrix()), type = "list")
  file <- tempfile(fileext = ".csv")
  ##tests adding the *.csv ending
  expect_type(create_ThetaMatrix(output_file = tempfile()), type = "list")
  expect_type(df <- create_ThetaMatrix(output_file = file), type = "list")

  ##add some lines to create the needed file
  df <- rbind(df,df)
  df[] <- 1
  input_file <- tempfile(fileext = ".csv")
  write.table(x = df, file = input_file, sep = ",", col.names = TRUE, row.names = FALSE)

  ##execute
  ##standard - data.frame to test warning
  expect_warning(create_ThetaMatrix(input = df, sigma_s = sigma_s))

  #standard import from file
  expect_type(
    suppressWarnings(create_ThetaMatrix(input = input_file, sigma_s = sigma_s)), "double")

  #standard export to file
  expect_type(suppressWarnings(
    create_ThetaMatrix(input = df, output_file = tempfile(), sigma_s = sigma_s)),
              type = "double")

  ##run with sigma_s NULL
  expect_type(suppressWarnings(create_ThetaMatrix(input = df, sigma_s = NULL)), "double")

  ##add NA
  df[1,1] <- NA
  df$DR_GAMMA_TOTAL <- 0
  expect_warning(create_ThetaMatrix(input = df, sigma_s = sigma_s))

  ##crash for non expected columns
  colnames(df) <- ""
  expect_error(create_ThetaMatrix(input = df, sigma_s = sigma_s))

})

test_that("check output",{
  skip_on_cran()
  local_edition(3)

  parms <- RLumModel:::.set_pars("Bailey2001")
  n <- parms$n$n
  test_simulate_RF_and_heating <- RLumModel:::.simulate_RF_and_heating(
    temp_begin = 20,
    temp_end = 500,
    heating_rate = 1,
    dose_rate = 0.06,
    n = n,
    parms = parms)

  expect_equal(class(test_simulate_RF_and_heating)[1], "RLum.Results")

  expect_equal(length(test_simulate_RF_and_heating$n), length(parms$N) + 2)

  expect_equal(test_simulate_RF_and_heating$temp, 500)

  ##check concentrations
  expect_equal(length(test_simulate_RF_and_heating$concentrations), length(parms$N) + 2)

  expect_equal(class(test_simulate_RF_and_heating$concentrations), "list")

  expect_equal(class(test_simulate_RF_and_heating$concentrations[[1]])[1], "RLum.Data.Curve")

})

test_that("check output Bailey2002",{
  skip_on_cran()
  local_edition(3)

  parms <- RLumModel:::.set_pars("Bailey2002")
  n <- parms$n$n
  test_simulate_RF <- RLumModel:::.simulate_RF_and_heating(
    temp_begin = 20,
    temp_end = 30,
    heating_rate = 5,
    dose_rate = 1,
    n = n,
    parms = parms)


  expect_equal(class(test_simulate_RF)[1], "RLum.Results")

  expect_equal(length(test_simulate_RF$n), length(parms$N) + 2)

  expect_equal(test_simulate_RF$temp, 30)

  ##check concentrations
  expect_equal(length(test_simulate_RF$concentrations), length(parms$N) + 2)

  expect_equal(class(test_simulate_RF$concentrations), "list")

  expect_equal(class(test_simulate_RF$concentrations[[1]])[1], "RLum.Data.Curve")

})

test_that("check output Bailey2004",{
  skip_on_cran()
  local_edition(3)

  parms <- RLumModel:::.set_pars("Bailey2004")
  n <- parms$n$n
  test_simulate_RF <-  RLumModel:::.simulate_RF_and_heating(
    temp_begin = 20,
    temp_end = 30,
    heating_rate = 5,
    dose_rate = 1,
    n = n,
    parms = parms)


  expect_equal(class(test_simulate_RF)[1], "RLum.Results")

  expect_equal(length(test_simulate_RF$n), length(parms$N) + 2)

  expect_equal(test_simulate_RF$temp, 30)

  ##check concentrations
  expect_equal(length(test_simulate_RF$concentrations), length(parms$N) + 2)

  expect_equal(class(test_simulate_RF$concentrations), "list")

  expect_equal(class(test_simulate_RF$concentrations[[1]])[1], "RLum.Data.Curve")

})

test_that("check output Friedrich2018",{
  skip_on_cran()
  local_edition(3)

  parms <- RLumModel:::.set_pars("Friedrich2018")
  n <- parms$n$n
  test_simulate_RF <-  RLumModel:::.simulate_RF_and_heating(
    temp_begin = 20,
    temp_end = 30,
    heating_rate = 5,
    dose_rate = 1,
    n = n,
    parms = parms)



  expect_equal(class(test_simulate_RF)[1], "RLum.Results")

  expect_equal(length(test_simulate_RF$n), length(parms$N) + 2)

  expect_equal(test_simulate_RF$temp, 30)

  ##check concentrations
  expect_equal(length(test_simulate_RF$concentrations), length(parms$N) + 2)

  expect_equal(class(test_simulate_RF$concentrations), "list")

  expect_equal(class(test_simulate_RF$concentrations[[1]])[1], "RLum.Data.Curve")

})

test_that("test controlled crash conditions", {
  skip_on_cran()
  local_edition(3)

  parms <- RLumModel:::.set_pars("Friedrich2018")
  n <- parms$n$n
  test_simulate_RF <-  RLumModel:::.simulate_RF_and_heating(
    temp_begin = 20,
    temp_end = 30,
    heating_rate = 5,
    dose_rate = 1,
    n = n,
    parms = parms)


  expect_error(
     RLumModel:::.simulate_RF_and_heating(
      temp_begin = -274,
      temp_end = 10,
      heating_rate = 5,
      dose_rate = 1,
      n = n,
      parms = parms),
    regexp = "\n [.simulate_RF_and_heating()] Argument 'temp' has to be > 0 K!", fixed = TRUE)

  expect_error(
     RLumModel:::.simulate_RF_and_heating(
      temp_begin = 125,
      temp_end = 200,
      heating_rate = 5,
      dose_rate = -1,
      n = n,
      parms = parms),
    regexp = "\n [.simulate_RF_and_heating()] Argument 'dose_rate' has to be a positive number!", fixed = TRUE)


  expect_error(
     RLumModel:::.simulate_RF_and_heating(
      temp_begin = 100,
      temp_end = 80,
      heating_rate = 1,
      dose_rate = 1,
      n = n,
      parms = parms),
    regexp = "\n [.simulate_RF_and_heating()] Heatingrate has the wrong algebraic sign!", fixed = TRUE)

})


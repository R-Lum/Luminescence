test_that("check output",{
  skip_on_cran()
  local_edition(3)

  parms <- RLumModel:::.set_pars("Bailey2001")
  n <- parms$n$n
  test_simulate_RF <- expect_s4_class(RLumModel:::.simulate_RF(
    temp = 20,
    dose = 1,
    dose_rate = 1,
    n = n,
    parms = parms), "RLum.Results")

  expect_equal(length(test_simulate_RF$n), length(parms$N) + 2)

  expect_equal(test_simulate_RF$temp, 20)

  ##check concentrations
  expect_equal(length(test_simulate_RF$concentrations), length(parms$N) + 2)

  expect_equal(class(test_simulate_RF$concentrations), "list")

  expect_equal(class(test_simulate_RF$concentrations[[1]])[1], "RLum.Data.Curve")

})

test_that("check output Bailey2002",{
  skip_on_cran()
  local_edition(3)

  parms <- RLumModel:::.set_pars("Bailey2002")
  n <- parms$n$n
  test_simulate_RF <- expect_s4_class(RLumModel:::.simulate_RF(
    temp = 20,
    dose = 1,
    dose_rate = 1,
    n = n,
    parms = parms), "RLum.Results")

  expect_equal(length(test_simulate_RF$n), length(parms$N) + 2)

  expect_equal(test_simulate_RF$temp, 20)

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
  test_simulate_RF <- RLumModel:::.simulate_RF(
    temp = 20,
    dose = 1,
    dose_rate = 1,
    n = n,
    parms = parms)

  expect_equal(class(test_simulate_RF)[1], "RLum.Results")

  expect_equal(length(test_simulate_RF$n), length(parms$N) + 2)

  expect_equal(test_simulate_RF$temp, 20)

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
  test_simulate_RF <- RLumModel:::.simulate_RF(
    temp = 20,
    dose = 1,
    dose_rate = 1,
    n = n,
    parms = parms)

  expect_equal(class(test_simulate_RF)[1], "RLum.Results")

  expect_equal(length(test_simulate_RF$n), length(parms$N) + 2)

  expect_equal(test_simulate_RF$temp, 20)

  ##check concentrations
  expect_equal(length(test_simulate_RF$concentrations), length(parms$N) + 2)

  expect_equal(class(test_simulate_RF$concentrations), "list")

  expect_equal(class(test_simulate_RF$concentrations[[1]])[1], "RLum.Data.Curve")

})

test_that("check output for dose = 0",{
  skip_on_cran()
  local_edition(3)

  parms <- RLumModel:::.set_pars("Bailey2001")
  n <- parms$n$n
  test_simulate_RF <- RLumModel:::.simulate_RF(
    temp = 20,
    dose = 0,
    dose_rate = 1,
    n = n,
    parms = parms)

  expect_equal(class(test_simulate_RF)[1], "RLum.Results")

  expect_equal(length(test_simulate_RF$n), length(parms$N) + 2)

  expect_equal(test_simulate_RF$temp, 20)

  ##check concentrations
  expect_equal(test_simulate_RF$concentrations, NULL)

})

test_that("test controlled crash conditions", {
  skip_on_cran()
  local_edition(3)

  expect_error(
    RLumModel:::.simulate_RF(
      temp = -274,
      dose = 1,
      dose_rate = 1,
      n = n,
      parms = parms),
    regexp = "\n [.simulate_RL()] Argument 'temp' has to be > 0 K!", fixed = TRUE)

  expect_error(
    RLumModel:::.simulate_RF(
      temp = 125,
      dose = 1,
      dose_rate = -1,
      n = n,
      parms = parms),
    regexp = "\n [.simulate_RL()] Argument 'dose_rate' has to be a positive number!", fixed = TRUE)


  expect_error(
    RLumModel:::.simulate_RF(
      temp = 125,
      dose = -1,
      dose_rate = 1,
      n = n,
      parms = parms),
    regexp = "\n [.simulate_RL()] Argument 'dose' has to be a positive number!", fixed = TRUE)

})




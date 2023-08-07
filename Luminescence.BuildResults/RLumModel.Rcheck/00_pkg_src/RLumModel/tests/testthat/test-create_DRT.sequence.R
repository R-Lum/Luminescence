test_that("check class of output",{
  skip_on_cran()
  local_edition(3)

  sequence <- RLumModel:::.create_DRT.sequence(
    Irr_2recover = 20,
    RegDose = c(0,8),
    TestDose = 5,
    PH = 240,
    CH = 200,
    OSL_temp = 125
  )

  expect_equal(class(sequence), "list")
})

test_that("check error for missing object",{
  skip_on_cran()
  local_edition(3)

  expect_error(
    sequence <- RLumModel:::.create_DRT.sequence(
      RegDose = c(0,8),
      TestDose = 5,
      PH = 240,
      CH = 200,
      OSL_temp = 125),
    "argument \"Irr_2recover\" is missing")

  expect_error(
    sequence <- RLumModel:::.create_DRT.sequence(
      Irr_2recover = 20,
      RegDose = c(0,8),
      TestDose = 5,
      PH = 240,
      CH = 200),
    "argument \"OSL_temp\" is missing")

  expect_error(
    sequence <- RLumModel:::.create_DRT.sequence(
      Irr_2recover = 20,
      RegDose = c(0,8),
      TestDose = 5,
      PH = 240,
      OSL_temp = 125),
    "argument \"CH\" is missing")

  expect_error(
    sequence <- RLumModel:::.create_DRT.sequence(
      Irr_2recover = 20,
      RegDose = c(0,8),
      TestDose = 5,
      OSL_temp = 125,
      CH = 200),
    "argument \"PH\" is missing")

  expect_error(
    sequence <- RLumModel:::.create_DRT.sequence(
      Irr_2recover = 20,
      RegDose = c(0,8),
      PH = 240,
      CH = 200,
      OSL_temp = 125),
    "argument \"TestDose\" is missing")

  expect_error(
    sequence <- RLumModel:::.create_DRT.sequence(
      Irr_2recover = 20,
      TestDose = 5,
      PH = 240,
      CH = 200,
      OSL_temp = 125),
    "argument \"RegDose\" is missing")


})

test_that("check length of output",{
  skip_on_cran()
  local_edition(3)

  expect_equal(length(RLumModel:::.set_pars("Bailey2001")), 12)
  expect_equal(length(RLumModel:::.set_pars("Bailey2002")), 12)
  expect_equal(length(RLumModel:::.set_pars("Bailey2004")), 12)
  expect_equal(length(RLumModel:::.set_pars("Pagonis2007")), 12)
  expect_equal(length(RLumModel:::.set_pars("Pagonis2008")), 12)
  expect_equal(length(RLumModel:::.set_pars("Friedrich2017")), 12)
  expect_equal(length(RLumModel:::.set_pars("customized")), 5)

})

test_that("check class of output",{
  skip_on_cran()
  local_edition(3)

  expect_equal(class(RLumModel:::.set_pars("Bailey2001")), "list")
  expect_equal(class(RLumModel:::.set_pars("Bailey2002")), "list")
  expect_equal(class(RLumModel:::.set_pars("Bailey2004")), "list")
  expect_equal(class(RLumModel:::.set_pars("Pagonis2007")), "list")
  expect_equal(class(RLumModel:::.set_pars("Pagonis2008")), "list")
  expect_equal(class(RLumModel:::.set_pars("Friedrich2017")), "list")
  expect_equal(class(RLumModel:::.set_pars("customized")), "list")

})

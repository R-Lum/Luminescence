test_that("chrash function and others",{
  skip_on_cran()
  local_edition(3)

  expect_error(.set_pars("error"),
               "\\[\\.set_Pars\\(\\)\\] Model not supported. Supported models are: Bailey2001, Bailey2004, Pagonis2008, Pagonis2007, Bailey2002, Friedrich2017, Friedrich2018, Peng2022, customized, customised")

  ## this should return a character vector with allowed keywords
  expect_type(.set_pars(), "character")

})

test_that("check length of output",{
  skip_on_cran()
  local_edition(3)

  expect_equal(length(.set_pars("Bailey2001")), 14)
  expect_equal(length(.set_pars("Bailey2002")), 14)
  expect_equal(length(.set_pars("Bailey2004")), 14)
  expect_equal(length(.set_pars("Pagonis2007")), 14)
  expect_equal(length(.set_pars("Pagonis2008")), 14)
  expect_equal(length(.set_pars("Friedrich2017")), 14)
  expect_equal(length(.set_pars("Peng2022")), 14)
  expect_equal(length(.set_pars("customized")), 7)
  expect_equal(length(.set_pars("customised")), 7)

})

test_that("check class of output",{
  skip_on_cran()
  local_edition(3)

  expect_equal(class(.set_pars("Bailey2001")), "list")
  expect_equal(class(.set_pars("Bailey2002")), "list")
  expect_equal(class(.set_pars("Bailey2004")), "list")
  expect_equal(class(.set_pars("Pagonis2007")), "list")
  expect_equal(class(.set_pars("Pagonis2008")), "list")
  expect_equal(class(.set_pars("Friedrich2017")), "list")
  expect_equal(class(.set_pars("Peng2022")), "list")
  expect_equal(class(.set_pars("customized")), "list")
  expect_equal(class(.set_pars("customised")), "list")

})

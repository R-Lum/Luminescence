context("set_pars")

test_that("check length of output",{
  
  expect_equal(length(.set_pars("Bailey2001")), 12)
  expect_equal(length(.set_pars("Bailey2002")), 12)
  expect_equal(length(.set_pars("Bailey2004")), 12)
  expect_equal(length(.set_pars("Pagonis2007")), 12)
  expect_equal(length(.set_pars("Pagonis2008")), 12)
  expect_equal(length(.set_pars("Friedrich2017")), 12)
  expect_equal(length(.set_pars("customized")), 5)
  
})

test_that("check class of output",{
  
  expect_equal(class(.set_pars("Bailey2001")), "list")
  expect_equal(class(.set_pars("Bailey2002")), "list")
  expect_equal(class(.set_pars("Bailey2004")), "list")
  expect_equal(class(.set_pars("Pagonis2007")), "list")
  expect_equal(class(.set_pars("Pagonis2008")), "list")
  expect_equal(class(.set_pars("Friedrich2017")), "list")
  expect_equal(class(.set_pars("customized")), "list")
  
})

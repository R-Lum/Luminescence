context("set parameters")

test_that("check inputs set_pars",{
  expect_output(.set_pars("Bailey2001"), "data: 12")
  expect_output(.set_pars("Bailey2002"), "data: 12")
  expect_output(.set_pars("Bailey2004"), "data: 12")
  expect_output(.set_pars("Pagonis2007"), "data: 12")
  expect_output(.set_pars("Pagonis2008"), "data: 12")
})

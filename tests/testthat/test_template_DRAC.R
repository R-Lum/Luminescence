context("template_DRAC")

##Full check
test_that("Check template creation ", {
  skip_on_cran()

  ##test success
  expect_is(template_DRAC(), "DRAC.list")
  expect_is(template_DRAC(notification = FALSE), "DRAC.list")
  expect_is(template_DRAC(nrow = 10, notification = FALSE), "DRAC.list")

})


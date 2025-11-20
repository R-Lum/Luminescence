test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(install_DevelopmentVersion("error"),
               "'force_install' should be a single logical value")
  expect_error(install_DevelopmentVersion(TRUE, FALSE),
               "'branch' should be of class 'character' and have length 1")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  SW({
  expect_message(install_DevelopmentVersion(FALSE, "master"),
                 "Please copy and run the following code in your R terminal")
  })
})

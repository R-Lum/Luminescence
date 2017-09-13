context("GitHub Interface")

test_that("Check github_commits()", {
  testthat::skip_on_cran()
  testthat::skip_on_os("mac")

  expect_is(github_commits(), 'data.frame')

})

test_that("Check github_branches()", {
  testthat::skip_on_cran()
  testthat::skip_on_os("mac")

  expect_is(github_branches(), 'data.frame')

})

test_that("Check github_issues()", {
  testthat::skip_on_cran()
  testthat::skip_on_os("mac")

  expect_output(github_issues())

})

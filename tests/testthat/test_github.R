test_that("Deprecated github functions", {
  testthat::skip_on_cran()

  SW({
  expect_warning(github_commits(),
                 "This function is deprecated")
  expect_warning(github_branches(),
                 "This function is deprecated")
  expect_warning(github_issues(),
                 "This function is deprecated")
  })
})

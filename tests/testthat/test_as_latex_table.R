test_that("Check github_commits()", {
  testthat::skip_on_cran()
  local_edition(3)

  df <- data.frame(x = "test", y = 1:10)
  expect_output(Luminescence:::.as.latex.table.data.frame(df))

})

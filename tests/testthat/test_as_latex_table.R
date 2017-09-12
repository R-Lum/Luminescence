context("Internal as LaTeX table")

test_that("Check github_commits()", {
  testthat::skip_on_cran()

  df <- data.frame(x = "test", y = 1:10)

  expect_output(Luminescence:::.as.latex.table.data.frame(df))

})

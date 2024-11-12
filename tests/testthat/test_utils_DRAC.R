test_that("utils DRAC", {
  testthat::skip_on_cran()

  ## generate test dataset with DRAC
  DRAC <- template_DRAC(preset = "quartz_coarse", notification = FALSE)
  t <- expect_type(.get_DRAC_references(DRAC), "list")
  expect_length(t, 2)

  ## check table
  DRAC <- as.data.frame(DRAC)
  t <- expect_type(.get_DRAC_references(DRAC), "list")
  expect_length(t, 2)

})


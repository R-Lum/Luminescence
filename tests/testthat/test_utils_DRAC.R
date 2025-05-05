test_that("check functionality", {
  testthat::skip_on_cran()

  ## generate test dataset with DRAC
  DRAC <- template_DRAC(preset = "quartz_coarse", notification = FALSE)
  t <- expect_type(.get_DRAC_references(DRAC), "list")
  expect_length(t, 2)
  expect_length(t$refs, 7)

  ## check table
  DRAC <- as.data.frame(DRAC)
  t <- expect_type(.get_DRAC_references(DRAC), "list")
  expect_length(t, 2)
  expect_length(t$refs, 7)

  ## more coverage
  DRAC[["TI:4"]] <- DRAC[["TI:34"]] <- DRAC[["TI:35"]] <- DRAC[["TI:38"]] <- "X"
  t <- expect_type(.get_DRAC_references(DRAC), "list")
  expect_length(t$refs, 3)
})

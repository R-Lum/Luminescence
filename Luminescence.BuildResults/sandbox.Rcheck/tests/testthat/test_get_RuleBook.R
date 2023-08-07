test_that("full example test", {
  testthat::skip_on_cran()
  local_edition(3)
  
  ## create empty rule book
  book_flat <- expect_type(get_RuleBook(book = "empty"), "list")
  
  ## check length (regression test)
  expect_length(book_flat, 6)
  
  ## check attributes
  expect_true(all(names(attributes(book_flat)) %in% c("names", "package", "medium")))
  
  ## check OSL option 
  book_osl <- expect_type(get_RuleBook(osl = "Bailey2001"), "list")
  expect_length(book_osl, 63)
  
})
  
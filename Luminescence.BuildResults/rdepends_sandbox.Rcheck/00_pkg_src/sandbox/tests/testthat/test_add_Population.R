test_that("full example test", {
  testthat::skip_on_cran()
  local_edition(3)
  
  ## prepare test
  book_1 <- get_RuleBook()
  
  ## simple test
  book_2 <- expect_type(add_Population(
    book = book_1, 
    populations = 1), "list")
  
  ## check the length
  expect_length(book_2, 6)
  
  ## check attributes
  expect_true(all(names(attributes(book_2)) %in% c("names", "package", "medium")))
  
  ## break function
  expect_error(add_Population(
    book = "error", 
    populations = 1), "\\[add_Population\\(\\)\\] 'book' is not an object created by sandbox!")
  
})
  
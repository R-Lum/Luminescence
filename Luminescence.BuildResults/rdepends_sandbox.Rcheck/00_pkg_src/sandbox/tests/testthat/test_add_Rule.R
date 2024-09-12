test_that("full example test", {
  testthat::skip_on_cran()
  local_edition(3)
  
  ## preparation
  book_1 <- get_RuleBook()
  
  ## simple test
  book_2 <- expect_type(add_Rule(
    book = book_1, 
    name = "extrarule", 
    group = "general", 
    type = "normal", 
    populations = 1), "list")
  
  ## check output
  expect_length(book_2, 7)
  
  ## check whether attributes were kept
  expect_true(all(names(attributes(book_2)) %in% c("names", "package", "medium")))
  
  ## break functions
  expect_error(add_Rule(
    book = "error", 
    name = "extrarule", 
    group = "general", 
    type = "normal", 
    populations = 1), "\\[add_Rule\\(\\)\\] 'book' is not an object created by sandbox!")
  
})
  
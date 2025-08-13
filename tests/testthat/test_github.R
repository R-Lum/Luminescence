## NOTE:
# Unauthenticated requests to the GiHub APIv3 are limited to 60 requests per hour
# (associated with the originating request). Exceeding the rate limit results in a
# 403 Forbidden reply. Since CIs make multiple requests when testing the rate limit
# is easily reached. We check whether we either get a valid response, or at least
# a 403 response.

test_that("Check github_commits()", {
  testthat::skip_on_cran()

  response <- tryCatch(github_commits(), error = function(e) return(e))

  expect_error(github_commits(user = NA),
               "'user' should be of class 'character'")
  expect_error(github_commits(user = letters),
               "'user' should have length 1")
  expect_error(github_commits(repo = NA),
               "'repo' should be of class 'character'")
  expect_error(github_commits(repo = letters),
               "'repo' should have length 1")
  expect_error(github_commits(branch = NA),
               "'branch' should be of class 'character'")
  expect_error(github_commits(branch = letters),
               "'branch' should have length 1")
  expect_error(github_commits(repo = "a test"),
               "URL using bad/illegal format or missing URL")
  expect_error(github_commits(n = -1:2),
               "'n' should be a positive integer scalar")

  if (inherits(response, "error")){
    expect_output(print(response), regexp = "returned status code")
  } else {
    expect_s3_class(response, "data.frame")
  }

  tryCatch(expect_error(github_commits(branch = "error"),
                        "Branch 'error' does not exist"),
           error = function(e) {
             expect_output(print(e), "returned status code")
           })
})

test_that("Check github_branches()", {
  testthat::skip_on_cran()

  expect_error(github_branches(user = NA),
               "'user' should be of class 'character'")
  expect_error(github_branches(user = letters),
               "'user' should have length 1")
  expect_error(github_branches(repo = NA),
               "'repo' should be of class 'character'")
  expect_error(github_branches(repo = letters),
               "'repo' should have length 1")
  expect_error(github_branches(repo = "a test"),
               "URL using bad/illegal format or missing URL")

  response <- tryCatch(github_branches(), error = function(e) return(e))

  if (inherits(response, "error")) {
    expect_output(print(response), regexp = "returned status code")
 }else {
    expect_s3_class(response, "data.frame")
 }

  rm(response)
})

test_that("Check github_issues()", {
  testthat::skip_on_cran()

  expect_error(github_issues(user = NA),
               "'user' should be of class 'character'")
  expect_error(github_issues(repo = NA),
               "'repo' should be of class 'character'")
  expect_error(github_issues(repo = "a test"),
               "URL using bad/illegal format or missing URL")

  SW({
  response <- tryCatch(github_issues(), error = function(e) return(e))
  })

  if (inherits(response, "error")){
    expect_output(print(response), regexp = "returned status code")
  }else{
    expect_type(response, "list")
  }

  rm(response)
})

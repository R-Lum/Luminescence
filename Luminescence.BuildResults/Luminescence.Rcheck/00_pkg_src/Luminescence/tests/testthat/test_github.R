## NOTE:
# Unauthenticated requests to the GiHub APIv3 are limited to 60 requests per hour
# (associated with the originating request). Exceeding the rate limit results in a
# 403 Forbidden reply. Since CIs make multiple requests when testing the rate limit
# is easily reached. We check whether we either get a valid response, or at least
# a 403 response.

test_that("Check github_commits()", {
  testthat::skip_on_cran()

  response <- tryCatch(github_commits(), error = function(e) return(e))

  if (inherits(response, "error")){
    expect_output(print(response), regexp = "status code 403")
  } else {
    expect_s3_class(response, "data.frame")
  }

  rm(response)
})

test_that("Check github_branches()", {
  testthat::skip_on_cran()

  response <- tryCatch(github_branches(), error = function(e) return(e))

  if (inherits(response, "error")) {
    expect_output(print(response), regexp = "status code 403")
 }else {
    expect_s3_class(response, "data.frame")
 }

  rm(response)
})

test_that("Check github_issues()", {
  testthat::skip_on_cran()

  SW({
  response <- tryCatch(github_issues(), error = function(e) return(e))
  })

  if (inherits(response, "error")){
    expect_output(print(response), regexp = "status code 403")
  }else{
    expect_type(response, "list")
  }

  rm(response)
})

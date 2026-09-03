## load data
data(ExampleData.DeValues, envir = environment())
res <- calc_CentralDose(ExampleData.DeValues$CA1,
                        plot = FALSE, verbose = FALSE)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(merge_RLum.Results("error"),
               "'objects' should be of class 'list'")
  expect_error(merge_RLum.Results(list(res, "error")),
               "All elements of 'object' should be of class 'RLum.Results'")
  expect_message(expect_null(merge_RLum.Results(list())),
                 "'objects' contains no data, NULL returned")
  expect_error(merge_RLum.Results(list(res), flatten = NA),
               "'flatten' should be a single logical value")

  res2 <- res
  res2@originator <- "unknown"
  expect_error(merge_RLum.Results(list(res, res2)),
               "Objects cannot be merged, different originators found: 'calc_CentralDose', 'unknown'")

  res2 <- res
  res2@data[[1]][, 2] <- NULL
    expect_error(merge_RLum.Results(list(res, res2)),
               "Objects cannot be merged, different number of columns")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  set.seed(1)
  a <- array(runif(300, 0,255), c(10,10,3))
  roi <- matrix(c(2.,4,2,5,6,7,3,1,1), ncol = 3)
  expect_snapshot_RLum(merge_RLum.Results(lapply(list(roi, roi, roi),
                                                 function(x) extract_ROI(a, x))))

  expect_snapshot_RLum(merge_RLum.Results(list(res, res)))

  empty <- set_RLum("RLum.Results")
  expect_snapshot_RLum(merge_RLum.Results(list(empty)))
  expect_s4_class(merge_RLum.Results(list(empty, empty)),
                  "RLum.Results")

  expect_snapshot_RLum(merge_RLum.Results(lapply(list(roi, roi),
                                                 function(x) extract_ROI(a, x)),
                                          flatten = FALSE))

  ## vector elements
  r1 <- set_RLum("RLum.Results", data = list(res = c(1, 2)))
  r2 <- set_RLum("RLum.Results", data = list(res = c(3, 4, 5)))
  expect_snapshot_RLum(merge_RLum.Results(list(r1, r2)))

  ## matrix with a custom attribute
  m1 <- matrix(1:4, nrow = 2); attr(m1, "myattr") <- "a"
  m2 <- matrix(5:8, nrow = 2); attr(m2, "myattr") <- "b"
  r1 <- set_RLum("RLum.Results", data = list(res = m1))
  r2 <- set_RLum("RLum.Results", data = list(res = m2))
  out <- merge_RLum.Results(list(r1, r2))@data$res
  expect_identical(attributes(out)$myattr,
                   c("a", "b"))
  attr(out, "myattr") <- NULL
  expect_equal(out, rbind(m1, m2))

  ## data.frame with a custom attribute and row names
  d1 <- data.frame(a = 1, b = 2); attr(d1, "myattr") <- "x"; row.names(d1) <- "A"
  d2 <- data.frame(a = 3, b = 4); attr(d2, "myattr") <- "y"; row.names(d2) <- "B"
  r1 <- set_RLum("RLum.Results", data = list(res = d1))
  r2 <- set_RLum("RLum.Results", data = list(res = d2))
  out <- merge_RLum.Results(list(r1, r2))@data$res
  expect_identical(attributes(out)$myattr,
                   c("x", "y"))
  expect_identical(rownames(out), c("1", "2"))
})

## load data
bin.v8 <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
risoe <- read_BIN2R(bin.v8, verbose = FALSE)
SW({
analysis <- merge_RLum(Risoe.BINfileData2RLum.Analysis(risoe))
})
curve <- analysis@records[[1]]

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(replace_metadata(risoe, list()) <- 1,
               "'info_element' should be of class 'character'")
  expect_error(replace_metadata(curve, list()) <- 1,
               "'info_element' should be of class 'character'")
  expect_error(replace_metadata(risoe, "error") <- 1,
               "'info_element' not recognised, valid terms are")
  expect_error(replace_metadata(curve, "error") <- 1,
               "'info_element' not recognised, valid terms are")
  expect_error(replace_metadata(risoe, "SEL", subset = error == 99) <- 0,
               "Invalid 'subset' expression, valid terms are")
  expect_error(replace_metadata(curve, "SEL", subset = error == 99) <- 0,
               "Invalid 'subset' expression, valid terms are")
  expect_error(replace_metadata(risoe, "SEL", subset = ID + 99) <- 0,
               "'subset' should contain a logical expression")
  expect_error(replace_metadata(curve, "SEL", subset = ID + 99) <- 0,
               "'subset' should contain a logical expression")
  expect_message(replace_metadata(risoe, "SEL", subset = ID == 99) <- 0,
                 "'subset' expression produced an empty selection, nothing done")
  expect_message(replace_metadata(risoe, "SEL", subset = ID == NA) <- 0,
                 "'subset' expression produced an empty selection, nothing done")
  expect_message(replace_metadata(curve, "SEL", subset = SET == 99) <- 0,
                 "'subset' expression produced an empty selection, nothing done")
  expect_message(replace_metadata(curve, "SEL", subset = ID == NA) <- 0,
                 "'subset' expression produced an empty selection, nothing done")
})

test_that("check functionality for Risoe.BINfileData", {
  testthat::skip_on_cran()

  res <- risoe
  replace_metadata(res, "SEL") <- FALSE
  expect_equal(res@METADATA$SEL,
               rep(FALSE, nrow(res@METADATA)))
  replace_metadata(res, "LTYPE", subset = SET == 2 & POSITION == 1) <- "OSL"
  expect_equal(res@METADATA$LTYPE,
               c("OSL", "TL"))

  ## the original object is unchanged
  expect_equal(risoe@METADATA$SEL,
               rep(TRUE, nrow(res@METADATA)))
  expect_equal(risoe@METADATA$LTYPE,
               rep("TL", nrow(res@METADATA)))
})

test_that("check functionality for RLum.Analysis", {
  testthat::skip_on_cran()

  res <- analysis
  replace_metadata(res, "SEL") <- FALSE
  expect_equal(sapply(res@records, function(x) x@info[["SEL"]]),
               c(FALSE, FALSE))
  replace_metadata(res, "LTYPE", subset = SET == 2 & POSITION == 1) <- "OSL"
  expect_equal(sapply(res@records, function(x) x@info[["LTYPE"]]),
               c("OSL", "TL"))

  ## the original object is unchanged
  expect_equal(sapply(analysis@records, function(x) x@info[["SEL"]]),
               rep(TRUE, length(res@records)))
  expect_equal(sapply(analysis@records, function(x) x@info[["LTYPE"]]),
               rep("TL", length(analysis@records)))
})

test_that("check functionality for RLum.Data", {
  testthat::skip_on_cran()

  res <- curve
  replace_metadata(res, "SEL") <- FALSE
  expect_equal(res@info$SEL, FALSE)
  replace_metadata(res, "LTYPE", subset = SET == 2) <- "OSL"
  expect_equal(res@info$LTYPE, "OSL")

  ## the original object is unchanged
  expect_equal(curve@info$SEL, TRUE)
  expect_equal(curve@info$LTYPE, "TL")
})

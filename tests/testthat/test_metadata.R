## load data
bin.v8 <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
risoe <- read_BIN2R(bin.v8, verbose = FALSE)
SW({
analysis <- merge_RLum(Risoe.BINfileData2RLum.Analysis(risoe))
})
curve <- analysis@records[[1]]

test_that("input validation", {
  testthat::skip_on_cran()

  ## add_metadata
  expect_error(add_metadata(risoe, list()) <- 1,
               "'info_element' should be of class 'character'")
  expect_error(add_metadata(curve, list()) <- 1,
               "'info_element' should be of class 'character'")
  expect_error(add_metadata(risoe, c("VAL1", "VAL2")) <- 1,
               "'info_element' should have length 1")
  expect_error(add_metadata(curve, c("VAL1", "VAL2")) <- 1,
               "'info_element' should have length 1")
  expect_error(add_metadata(risoe, "POSITION") <- 1,
               "'info_element' already present, to modify it you should use")
  expect_error(add_metadata(curve, "POSITION") <- 1,
               "'info_element' already present, to modify it you should use")

  ## rename_metadata
  expect_error(rename_metadata(risoe, list()) <- 1,
               "'info_element' should be of class 'character'")
  expect_error(rename_metadata(curve, list()) <- 1,
               "'info_element' should be of class 'character'")
  expect_error(rename_metadata(risoe, c("VAL1", "VAL2")) <- 1,
               "'info_element' should have length 1")
  expect_error(rename_metadata(curve, c("VAL1", "VAL2")) <- 1,
               "'info_element' should have length 1")
  expect_error(rename_metadata(risoe, "error") <- 1,
               "'info_element' not recognised ('error'), valid terms are",
               fixed = TRUE)
  expect_error(rename_metadata(curve, "error") <- 1,
               "'info_element' not recognised ('error'), valid terms are",
               fixed = TRUE)

  ## replace_metadata
  expect_error(replace_metadata(risoe, list()) <- 1,
               "'info_element' should be of class 'character'")
  expect_error(replace_metadata(curve, list()) <- 1,
               "'info_element' should be of class 'character'")
  expect_error(replace_metadata(risoe, "error") <- 1,
               "'info_element' not recognised ('error'), valid terms are",
               fixed = TRUE)
  expect_error(replace_metadata(curve, "error") <- 1,
               "'info_element' not recognised ('error'), valid terms are",
               fixed = TRUE)
  expect_error(replace_metadata(risoe, "SEL", subset = POSITION == 2) <- NULL,
               "'subset' is incompatible with assigning NULL")
  expect_error(replace_metadata(curve, "SEL", subset = POSITION == 2) <- NULL,
               "'subset' is incompatible with assigning NULL")
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

  ## add_metadata
  add_metadata(res, "NEW") <- 123
  expect_equal(res@METADATA$NEW,
               rep(123, nrow(res@METADATA)))

  ## rename_metadata
  rename_metadata(res, "NEW") <- "NEWER"
  expect_equal(res@METADATA$NEWER,
               rep(123, nrow(res@METADATA)))

  ## replace_metadata
  replace_metadata(res, "SEL") <- FALSE
  expect_equal(res@METADATA$SEL,
               rep(FALSE, nrow(res@METADATA)))
  replace_metadata(res, "LTYPE", subset = SET == 2 & POSITION == 1) <- "OSL"
  expect_equal(res@METADATA$LTYPE,
               c("OSL", "TL"))
  replace_metadata(res, c("PTENABLED", "DTENABLED")) <- NULL
  expect_null(res@METADATA$PTENABLED)
  expect_null(res@METADATA$DTENABLED)

  ## the original object is unchanged
  expect_null(risoe@METADATA$NEW)
  expect_null(risoe@METADATA$NEWER)
  expect_equal(risoe@METADATA$SEL,
               rep(TRUE, nrow(res@METADATA)))
  expect_equal(risoe@METADATA$LTYPE,
               rep("TL", nrow(res@METADATA)))
  expect_equal(risoe@METADATA$PTENABLED,
               c(0, 0))
  expect_equal(risoe@METADATA$DTENABLED,
               c(0, 0))
})

test_that("check functionality for RLum.Analysis", {
  testthat::skip_on_cran()

  res <- analysis
  num.records <- length(analysis@records)

  ## add_metadata
  add_metadata(res, "NEW") <- 123
  expect_equal(sapply(res@records, function(x) x@info[["NEW"]]),
               c(123, 123))

  ## rename_metadata
  rename_metadata(res, "NEW") <- "NEWER"
  expect_equal(sapply(res@records, function(x) x@info[["NEWER"]]),
               c(123, 123))

  ## replace_metadata
  replace_metadata(res, "SEL") <- FALSE
  expect_equal(sapply(res@records, function(x) x@info[["SEL"]]),
               c(FALSE, FALSE))
  replace_metadata(res, "LTYPE", subset = SET == 2 & POSITION == 1) <- "OSL"
  expect_equal(sapply(res@records, function(x) x@info[["LTYPE"]]),
               c("OSL", "TL"))
  replace_metadata(res, "SEQUENCE") <- NULL
  expect_null(unlist(sapply(res@records, function(x) x@info[["SEQUENCE"]])))

  ## the original object is unchanged
  expect_null(unlist(sapply(analysis@records, function(x) x@info[["NEW"]])))
  expect_null(unlist(sapply(analysis@records, function(x) x@info[["NEWER"]])))
  expect_equal(sapply(analysis@records, function(x) x@info[["SEL"]]),
               rep(TRUE, num.records))
  expect_equal(sapply(analysis@records, function(x) x@info[["LTYPE"]]),
               rep("TL", num.records))
  expect_equal(sapply(analysis@records, function(x) x@info[["SEQUENCE"]]),
               rep("", num.records))
})

test_that("check functionality for RLum.Data", {
  testthat::skip_on_cran()

  res <- curve

  ## add_metadata
  add_metadata(res, "NEW") <- 123
  expect_equal(res@info$NEW, 123)

  ## rename_metadata
  rename_metadata(res, "NEW") <- "NEWER"
  expect_equal(res@info$NEWER, 123)

  ## replace_metadata
  replace_metadata(res, "SEL") <- FALSE
  expect_equal(res@info$SEL, FALSE)
  replace_metadata(res, "LTYPE", subset = SET == 2) <- "OSL"
  expect_equal(res@info$LTYPE, "OSL")
  replace_metadata(res, c("AN_TEMP", "AN_TIME")) <- NULL
  expect_null(res@info$AN_TEMP)
  expect_null(res@info$AN_TIME)

  ## the original object is unchanged
  expect_null(curve@info$NEW)
  expect_null(curve@info$NEWER)
  expect_equal(curve@info$SEL, TRUE)
  expect_equal(curve@info$LTYPE, "TL")
  expect_equal(curve@info$AN_TEMP, 220)
  expect_equal(curve@info$AN_TIME, 10)
})

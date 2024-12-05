## load data
bin.v8 <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
risoe <- read_BIN2R(bin.v8, verbose = FALSE)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(replace_metadata(risoe, "error") <- 1,
               "'info_element' not recognised")
  expect_error(replace_metadata(risoe, "SEL", subset = error == 99) <- 0,
               "Invalid 'subset' expression, valid terms are")
  expect_error(replace_metadata(risoe, "SEL", subset = ID + 99) <- 0,
               "'subset' should contain a logical expression")
  expect_message(replace_metadata(risoe, "SEL", subset = ID == 99) <- 0,
                 "'subset' expression produced an empty selection, nothing done")
  expect_message(replace_metadata(risoe, "SEL", subset = ID == NA) <- 0,
                 "'subset' expression produced an empty selection, nothing done")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## Risoe.BINfileData
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

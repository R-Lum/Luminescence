## load data
data(ExampleData.BINfileData, envir = environment())

## replace the with numeric and factors with characters
CWOSL.SAR.Data@METADATA$VERSION <- as.numeric(CWOSL.SAR.Data@METADATA$VERSION)
CWOSL.SAR.Data@METADATA[] <- lapply(CWOSL.SAR.Data@METADATA,
                                    function(x) {
                                      if (is.factor(x)) as.character(x) else x
                                    })

## combine with existing BIN-file object
empty <- set_Risoe.BINfileData()
new <- data.table::rbindlist(list(empty@METADATA, CWOSL.SAR.Data@METADATA),
                             fill = TRUE)
new <- set_Risoe.BINfileData(METADATA = as.data.frame(new, stringsAsFactors = FALSE),
                             DATA = CWOSL.SAR.Data@DATA)
new@METADATA[is.na(new@METADATA)] <- 0
new@METADATA$RECTYPE <- 1
new <- subset(new, ID == 1:2)

test_that("check functionality", {
  testthat::skip_on_cran()

  ##create files
  path <- tempfile()
  SW({
  write_R2BIN(object = new, file = paste0(path, "BINfile_V3.bin"), version = "03")
  write_R2BIN(object = new, file = paste0(path, "BINfile_V4.bin"), version = "04")
  write_R2BIN(object = new, file = paste0(path, "BINfile_V5.binx"), version = "05")
  write_R2BIN(object = new, file = paste0(path, "BINfile_V6.binx"), version = "06")
  write_R2BIN(object = new, file = paste0(path, "BINfile_V7.binx"), version = "07")
  write_R2BIN(object = new, file = paste0(path, "BINfile_V8.binx"), version = "08")
  })

  temp <- new
  temp@METADATA[1, "TIME"] <- "1215"
  temp@METADATA[1, "SEQUENCE"] <- "123456"
  temp@METADATA[1, "SAMPLE"] <- ""
  temp@METADATA[1, "COMMENT"] <- ""
  temp@.RESERVED <- list(val1 = c("a", "b"), val2 = c("c", "d"))
  expect_silent(
  write_R2BIN(object = temp, file = paste0(path, "BINfile_V3.bin"),
              version = "03", verbose = FALSE))
  SW({
  temp@METADATA[, "VERSION"] <- 4
  write_R2BIN(object = temp, file = paste0(path, "BINfile_V4.bin"),
              version = "04")
  temp@METADATA[, "VERSION"] <- 5
  write_R2BIN(object = temp, file = paste0(path, "BINfile_V5.binx"),
              version = "05")
  temp@METADATA[, "VERSION"] <- 6
  write_R2BIN(object = temp, file = paste0(path, "BINfile_V6.binx"),
              version = "06")
  temp@METADATA[, "VERSION"] <- 7
  write_R2BIN(object = temp, file = paste0(path, "BINfile_V7.binx"),
              version = "07")
  temp@METADATA[1, "TIME"] <- NA
  temp@METADATA[, "VERSION"] <- 8
  write_R2BIN(object = temp, file = paste0(path, "BINfile_V8.binx"),
              version = "08")

  ## trigger edge case
  temp@METADATA[, "FNAME"] <- numeric()
  expect_silent(write_R2BIN(object = temp, file = paste0(path, "BINfile_V8.binx"),
              version = "08", verbose = FALSE))
  })

  ## silent correction of the file extension
  SW({
  skip_on_os("windows") # FIXME(mcol): see commit 26889a6
  write_R2BIN(object = new, file = paste0(path, "BINfile_V8.bin"),
              version = "08")
  })

  ## check UTF-8 characters
  new_utf8 <- new
  new_utf8@METADATA$FNAME <- c("I do not belong in here \xb5m")
  t <- expect_silent(write_R2BIN(new_utf8, file = paste0(path, "BINfile_V8.bin"),
                     version = "08", verbose = FALSE))
  expect_type(object = t, type = "character")

})

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(write_R2BIN(object = new, file = FALSE),
               "'file' should be of class 'character'")
  expect_error(write_R2BIN(object = "a", file = ""),
               "'object' should be of class 'Risoe.BINfileData'")
  expect_error(suppressWarnings(write_R2BIN(object = set_Risoe.BINfileData(), file = "")))

  temp <- new
  temp@METADATA <- temp@METADATA[, 1:79]
  expect_error(write_R2BIN(object = temp, file = "test"),
               "Your Risoe.BINfileData object is not compatible with the latest")

  temp <- new
  temp@METADATA[1, "SEQUENCE"] <- "1234567890"
  expect_error(write_R2BIN(object = temp, file = "test"),
               "Value in 'SEQUENCE' exceeds storage limit")

  temp <- new
  temp@METADATA[1, "USER"] <- "1234567890"
  expect_error(write_R2BIN(object = temp, file = "test"),
               "'USER' exceeds storage limit")

  temp <- new
  temp@METADATA[1, "SAMPLE"] <- paste0(rep("a", 25), collapse="")
  expect_error(write_R2BIN(object = temp, file = "test"),
               "'SAMPLE' exceeds storage limit")

  temp <- new
  temp@DATA[[2]] <- 1:25000
  temp@METADATA[1, "POSITION"] <- paste0(rep("a", 50), collapse="")
  expect_error(write_R2BIN(object = temp, file = "test"),
               "records contain more than 9,999 data points")
  expect_warning(
    expect_error(write_R2BIN(object = temp, compatibility.mode = TRUE,
                             file = paste0(tempfile(), "BINfile_V8.binx")),
                 "'COMMENT' exceeds storage limit"),
    "some data sets have more than 9,999 points")

})


test_that("write to empty connection", {
  testthat::skip_on_cran()

#Unit test for write_BIN2R() function

#create data file
data(ExampleData.BINfileData, envir = environment())

  ##empty RisoeBINfileData object
  empty <- set_Risoe.BINfileData()

  ##replace the raw by numeric
  CWOSL.SAR.Data@METADATA$VERSION <- as.numeric(CWOSL.SAR.Data@METADATA$VERSION)
  CWOSL.SAR.Data@METADATA[] <- lapply(CWOSL.SAR.Data@METADATA, function(x){
    if(is.factor(x)){
      as.character(x)
    }else{
      x
    }
  })

  ##combing with existing BIN-file object
  new <- as.data.frame(
    data.table::rbindlist(l = list(empty@METADATA,CWOSL.SAR.Data@METADATA),fill = TRUE),
    stringsAsFactors = FALSE)

  ##new object
  new <- set_Risoe.BINfileData(METADATA = new, DATA = CWOSL.SAR.Data@DATA)

  ##replace NA values
  new@METADATA[is.na(new@METADATA)] <- 0

  ##replace RECTYPE
  new@METADATA$RECTYPE <- 1

  ##reduce files size considerably down to two records
  new <- subset(new, ID == 1:2)

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
  SW({
  write_R2BIN(object = temp, file = paste0(path, "BINfile_V3.bin"),
              version = "03")
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
  })

  ##catch errors
  expect_error(write_R2BIN(object = new, file = FALSE),
               "argument 'file' has to be of type character")
  expect_error(write_R2BIN(object = "a", file = ""), "[write_R2BIN()] Input object is not of type Risoe.BINfileData!", fixed = TRUE)
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
                             file = paste0(path, "BINfile_V8.binx")),
                 "'COMMENT' exceeds storage limit"),
    "Some data sets are longer than 9,999 points")

  ## silent correction of the file extension
  SW({
  skip_on_os("windows") # FIXME(mcol)
  write_R2BIN(object = new, file = paste0(path, "BINfile_V8.bin"),
              version = "08")
  })
})

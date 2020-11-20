test_that("write to empty connection", {
  testthat::skip_on_cran()
  local_edition(3)

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
  write_R2BIN(object = new, file = paste0(path, "BINfile_V3.bin"), version = "03")
  write_R2BIN(object = new, file = paste0(path, "BINfile_V4.bin"), version = "04")
  write_R2BIN(object = new, file = paste0(path, "BINfile_V65binx"), version = "05")
  write_R2BIN(object = new, file = paste0(path, "BINfile_V6.binx"), version = "06")
  write_R2BIN(object = new, file = paste0(path, "BINfile_V7.binx"), version = "07")
  write_R2BIN(object = new, file = paste0(path, "BINfile_V8.binx"), version = "08")

  ##catch errors
  expect_error(write_R2BIN(object = "a", file = ""), "[write_R2BIN()] Input object is not of type Risoe.BINfileData!", fixed = TRUE)
  expect_error(suppressWarnings(write_R2BIN(object = set_Risoe.BINfileData(), file = "")))

})


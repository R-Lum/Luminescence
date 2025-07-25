#' @title Export Risoe.BINfileData into Risø BIN/BINX-file
#'
#' @description Exports a `Risoe.BINfileData` object in a `*.bin` or `*.binx` file that can be
#' opened by the Analyst software or other Risø software.
#'
#' @details
#' The structure of the exported binary data follows the data structure
#' published in the Appendices of the *Analyst* manual p. 42.
#'
#' If
#' `LTYPE`, `DTYPE` and `LIGHTSOURCE` are not of type
#' [character], no transformation into numeric values is done.
#'
#' @param object [Risoe.BINfileData-class] (**required**):
#' input object to be stored in a bin file.
#'
#' @param file [character] (**required**):
#' file name and path of the output file
#'
#' - `[WIN]`: `write_R2BIN(object, "C:/Desktop/test.bin")`
#' - `[MAC/LINUX]`: `write_R2BIN("/User/test/Desktop/test.bin")`
#'
#' @param version [character] (*optional*):
#' version number for the output file. If no value is provided, the highest
#' version number from the [Risoe.BINfileData-class] is taken automatically.
#'
#' **Note:**
#' This argument can be used to convert BIN-file versions.
#'
#' @param compatibility.mode [logical] (*with default*):
#' this option recalculates the position values if necessary and set the max.
#' value to 48. The old position number is appended as comment (e.g., 'OP: 70).
#' This option accounts for potential compatibility problems with the Analyst software.
#' It further limits the maximum number of points per curve to 9,999. If a curve contains more
#' data the curve data get binned using the smallest possible bin width.
#'
#' @param verbose [logical] (*with default*):
#' enable/disable output to the terminal.
#'
#' @param txtProgressBar [logical] (*with default*):
#' enable/disable the progress bar. Ignored if `verbose = FALSE`.
#'
#' @return Write a binary file and returns the name and path of the file as [character].
#'
#' @note
#' The function just roughly checks the data structures. The validity of
#' the output data depends on the user.
#'
#' The validity of the file path is not further checked. BIN-file conversions
#' using the argument `version` may be a lossy conversion, depending on the
#' chosen input and output data (e.g., conversion from version 08 to 07 to 06 to 05 to 04 or 03).
#'
#' **Warning**
#'
#' Although the coding was done carefully, it seems that the BIN/BINX-files
#' produced by Risø DA 15/20 TL/OSL readers slightly differ on the byte level.
#' No obvious differences are observed in the METADATA, however, the
#' BIN/BINX-file may not fully compatible, at least not similar to the ones
#' directly produced by the Risø readers!
#'
#' @section Function version: 0.5.4
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @note
#' ROI definitions (introduced in BIN-file version 8) are not supported!
#' There are furthermore ignored by the function [read_BIN2R].
#'
#' @seealso [read_BIN2R], [Risoe.BINfileData-class], [writeBin]
#'
#' @references
#' DTU Nutech, 2016. The Sequence Editor, Users Manual, February, 2016.
#' [https://www.fysik.dtu.dk]()
#'
#' @keywords IO
#'
#' @examples
#' ##load example dataset
#' file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
#' temp <- read_BIN2R(file)
#'
#' ##create temporary file path
#' ##(for usage replace by own path)
#' temp_file <- tempfile(pattern = "output", fileext = ".binx")
#'
#' ##export to temporary file path
#' write_R2BIN(temp, file = temp_file)
#'
#' @export
write_R2BIN <- function(
  object,
  file,
  version,
  compatibility.mode = FALSE,
  verbose = TRUE,
  txtProgressBar = TRUE
) {
  .set_function_name("write_R2BIN")
  on.exit(.unset_function_name(), add = TRUE)

  # Config ------------------------------------------------------------------
  ##set supported BIN format version
  VERSION.supported <- as.raw(c(3, 4, 5, 6, 7, 8))

  ## Integrity tests --------------------------------------------------------

  .validate_class(object, "Risoe.BINfileData")
  .validate_class(file, "character")
  .validate_logical_scalar(verbose)
  .validate_logical_scalar(txtProgressBar)

  ## check if it fulfills the latest definition ...
  if(ncol(object@METADATA) != ncol(set_Risoe.BINfileData()@METADATA)){
    .throw_error("Your Risoe.BINfileData object is not compatible with the ",
                 "latest specification of this S4-class object. You are ",
                 "probably trying to export a Risoe.BINfileData from your ",
                 "workspace you produced manually or with an old version. ",
                 "Please re-import the BIN-file using function read_BIN2R().")
  }

  # Check Risoe.BINfileData Struture ----------------------------------------
  ##check whether the BIN-file DATA slot contains more than 9999 records; needs to be run all the time
  temp_check <- vapply(object@DATA, function(x) length(x) > 9999,
                       FUN.VALUE = logical(1))

  ##force compatibility
  if(compatibility.mode && any(temp_check)){
    .throw_warning("Compatibility mode selected: some data sets have ",
                   "more than 9,999 points and will be binned")

    ##BIN data to reduce amount of data if the BIN-file is too long
    object@DATA <- lapply(object@DATA, function(x){
      if(length(x) > 9999){
        ##we want to have a minimum binning (smallest number possible)
        bin_width <- ceiling(length(x)/9999)

        ##it should be symmetric, thus, remove values
        if((length(x)/bin_width)%%2 != 0){
          x <- x[-length(x)]
        }

        ##create matrix and return
        colSums(matrix(x, nrow = bin_width))

      }else{
        x
      }
    })

    ##reset temp_check
    temp_check <- FALSE

    ##get new number of points
    temp_NPOINTS <- lengths(object@DATA)

    ##correct LENGTH
    object@METADATA[["LENGTH"]] <- object@METADATA[["LENGTH"]] - (4 * object@METADATA[["NPOINTS"]]) + (temp_NPOINTS * 4)

    ##correct PREVIOUS
    object@METADATA[["PREVIOUS"]] <- c(0, head(object@METADATA$LENGTH, -1))

    ##correct NPOINTS
    object@METADATA[["NPOINTS"]] <- temp_NPOINTS

    ##write comment
    object@METADATA[["COMMENT"]] <- paste(object@METADATA[["COMMENT"]], " - binned")
  }

  if(any(temp_check))
    .throw_error(length(which(temp_check)), " out of ", length(temp_check),
                 " records contain more than 9,999 data points. ",
                 "This violates the BIN/BINX-file definition")

  ##remove
  rm(temp_check)

  ## UTF-8 conversion
  object@METADATA[["SAMPLE"]] <- base::iconv(object@METADATA[["SAMPLE"]], "latin1", "ASCII", sub="_")
  object@METADATA[["COMMENT"]] <- base::iconv(object@METADATA[["COMMENT"]], "latin1", "ASCII", sub="_")
  object@METADATA[["FNAME"]] <- base::iconv(object@METADATA[["FNAME"]], "latin1", "ASCII", sub="_")
  object@METADATA[["USER"]] <- base::iconv(object@METADATA[["USER"]], "latin1", "ASCII", sub="_")
  object@METADATA[["SEQUENCE"]] <- base::iconv(object@METADATA[["SEQUENCE"]], "latin1", "ASCII", sub="_")

  ##VERSION
  ##If missing version argument set to the highest value
  if(missing(version)){
    version <- as.raw(max(as.numeric(object@METADATA[,"VERSION"])))
    version.original <- version

  }else{
    version.original <- as.raw(max(as.numeric(object@METADATA[,"VERSION"])))
    version <- as.raw(version)
    object@METADATA[["VERSION"]] <- version

    ##Furthermore, entries length needed to be recalculated
    if(version.original != version){
      ##stepping decision
      header.stepping <- switch(
        EXPR = as.character(version),
        "08" = 507,
        "07" = 447,
        "06" = 447,
        "05" = 423,
        "04" = 272,
        "03" = 272)

      object@METADATA$LENGTH <- header.stepping + 4 * object@METADATA$NPOINTS
      object@METADATA$PREVIOUS <- c(0, head(object@METADATA$LENGTH, -1))
    }
  }

  ##Check if the BINfile object contains of unsupported versions
  if (!as.raw(object@METADATA$VERSION[1]) %in% VERSION.supported ||
      !version %in% VERSION.supported) {
    .throw_error("Writing BIN-files in format version (",
                 object@METADATA[1, "VERSION"], ") is currently not supported, ",
                 "supported version numbers are: ",
                 .collapse(VERSION.supported))
  }

  ## ensure that file name for version >= 05 has *.binx extension
  if (version >= 05 && tools::file_ext(file) == "bin") {
    file <- paste0(tools::file_path_sans_ext(file), ".binx")
  }

  ##SEQUENCE
  if (suppressWarnings(max(nchar(as.character(object@METADATA[,"SEQUENCE"]), type =
                                 "bytes"), na.rm = TRUE)) > 8) {
    .throw_error("Value in 'SEQUENCE' exceeds storage limit")
  }

  ##USER
  if (suppressWarnings(max(nchar(as.character(object@METADATA[,"USER"]), type =
                                 "bytes"), na.rm = TRUE)) > 8) {
    .throw_error("'USER' exceeds storage limit")
  }

  ##SAMPLE
  if (suppressWarnings(max(nchar(as.character(object@METADATA[,"SAMPLE"]), type =
                                 "bytes"), na.rm = TRUE)) > 20) {
    .throw_error("'SAMPLE' exceeds storage limit")
  }

  ## enables compatibility to the Analyst as the max value for POSITION becomes 48
  if(compatibility.mode){
    ##just do if position values > 48
    if(max(object@METADATA[,"POSITION"])>48){

      ##grep relevant IDs
      temp.POSITION48.id <- which(object@METADATA[,"POSITION"]>48)

      ##find unique values
      temp.POSITION48.unique <- unique(object@METADATA[temp.POSITION48.id,"POSITION"])

      ##set translation vector starting from 1 and ending at 48
      temp.POSITION48.new <- rep_len(1:48, length.out = length(temp.POSITION48.unique))

      ## recalculate POSITION and update comment
      for(i in 1:length(temp.POSITION48.unique)){
        idx <- object@METADATA[, "POSITION"] == temp.POSITION48.unique[i]
        object@METADATA[idx, "COMMENT"] <- paste0(object@METADATA$COMMENT[idx],
                                                  "OP:", object@METADATA$POSITION[idx])
        object@METADATA[idx, "POSITION"] <- temp.POSITION48.new[i]
      }
    }
  }

  ##COMMENT
  if(max(nchar(as.character(object@METADATA[,"COMMENT"]), type="bytes"))>80){
    .throw_error("'COMMENT' exceeds storage limit")
  }

  ## Convert strings to integers --------------------------------------------
  ## The ordering of these entries is important, as it allows us to convert
  ## character strings to the corresponding 0-based index (which must be an
  ## integer, hence the -1L below)

  LTYPE.Conv <- c("TL", "OSL", "IRSL", "M-IR", "M-VIS", "TOL", "TRPOSL",
                  "RIR", "RBR", "USER", "POSL", "SGOSL", "RL", "XRF")
  DTYPE.Conv <- c("Natural", "N+dose", "Bleach", "Bleach+dose",
                  "Natural (Bleach)", "N+dose (Bleach)", "Dose", "Background")
  LIGHT.Conv <- c("None", "Lamp", "IR diodes/IR Laser",
                  "Calibration LED", "Blue Diodes", "White light",
                  "Green laser (single grain)", "IR laser (single grain)")

  ##TRANSLATE VALUES IN METADATA
  ##LTYPE
  if (is.character(object@METADATA$LTYPE) || is.factor(object@METADATA$LTYPE)) {
    object@METADATA[, "LTYPE"] <- match(object@METADATA[, "LTYPE"],
                                        LTYPE.Conv) - 1L
  }

  ##DTYPE
  if (is.character(object@METADATA$DTYPE) || is.factor(object@METADATA$DTYPE)) {
    object@METADATA[, "DTYPE"] <- match(object@METADATA[, "DTYPE"],
                                        DTYPE.Conv) - 1L
  }

  ##LIGHTSOURCE
  if (is.character(object@METADATA$LIGHTSOURCE) ||
      is.factor(object@METADATA$LIGHTSOURCE)) {
    object@METADATA[,"LIGHTSOURCE"] <- match(object@METADATA[, "LIGHTSOURCE"],
                                             LIGHT.Conv) - 1L
  }

  ##TIME
  object@METADATA[, "TIME"] <- vapply(object@METADATA[["TIME"]], function(x) {
    if (is.na(x))
      return("000000")
    as.character(gsub(":", "", x))
  }, character(1))

  ##TAG and SEL
  ##in TAG information on the SEL are storred, here the values are copied to TAG
  ##before export
  object@METADATA[,"TAG"] <- ifelse(object@METADATA[,"SEL"] == TRUE, 1, 0)

  # SET FILE AND VALUES -----------------------------------------------------
  con <- file(file, "wb")
  on.exit(close(con), add = TRUE)

  ##get records
  n.records <- length(object@METADATA[,"ID"])

  ## don't show the progress bar if not verbose
  if (!verbose)
    txtProgressBar <- FALSE

  if (verbose) {
    cat("\n[write_R2BIN()] Exporting ...")
    cat("\n path: ", dirname(file))
    cat("\n file: ", .shorten_filename(basename(file)))
    cat("\n n_rec:", n.records)
    cat("\n")
  }

  ##set progressbar
  if(txtProgressBar)
    pb <- txtProgressBar(min=0,max=n.records, char="=", style=3)

  # LOOP -------------------------------------------------------------------
  ID <- 1
  if(version == 03 || version == 04){
    ## version 03 and 04
    ##start loop for export BIN data
    while(ID<=n.records) {

      ##VERSION
      writeBin(as.raw(object@METADATA[ID,"VERSION"]),
               con,
               size = 1,
               endian="little")

      ##stepping
      writeBin(raw(length=1),
               con,
               size = 1,
               endian="little")

      ##LENGTH, PREVIOUS, NPOINTS
      writeBin(c(as.integer(object@METADATA[ID,"LENGTH"]),
                 as.integer(object@METADATA[ID,"PREVIOUS"]),
                 as.integer(object@METADATA[ID,"NPOINTS"])),
               con,
               size = 2,
               endian="little")

      ##LTYPE
      writeBin(object@METADATA[ID,"LTYPE"],
               con,
               size = 1,
               endian="little")

      ##LOW, HIGH, RATE
      writeBin(c(as.double(object@METADATA[ID,"LOW"]),
                 as.double(object@METADATA[ID,"HIGH"]),
                 as.double(object@METADATA[ID,"RATE"])),
               con,
               size = 4,
               endian="little")

      ##TEMPERATURE, XCOORD, YCOORD, TOLDELAY; TOLON, TOLOFF
      writeBin(c(as.integer(object@METADATA[ID,"TEMPERATURE"]),
                 as.integer(object@METADATA[ID,"XCOORD"]),
                 as.integer(object@METADATA[ID,"YCOORD"]),
                 as.integer(object@METADATA[ID,"TOLDELAY"]),
                 as.integer(object@METADATA[ID,"TOLON"]),
                 as.integer(object@METADATA[ID,"TOLOFF"])),
               con,
               size = 2,
               endian="little")

      ##POSITION, RUN
      writeBin(c(as.integer(object@METADATA[ID,"POSITION"]),
                 as.integer(object@METADATA[ID,"RUN"])),
               con,
               size = 1,
               endian="little")

      ##TIME
      TIME_SIZE <- nchar(object@METADATA[ID,"TIME"])
      writeBin(as.integer(TIME_SIZE),
               con,
               size = 1,
               endian="little")

      writeChar(object@METADATA[ID,"TIME"],
                con,
                nchars =TIME_SIZE,
                useBytes=TRUE,
                eos = NULL)

      if(6-TIME_SIZE>0){
        writeBin(raw(length = c(6-TIME_SIZE)),
                 con,
                 size = 1,
                 endian="little")
      }

      ##DATE
      writeBin(as.integer(6),
               con,
               size = 1 ,
               endian="little")

      suppressWarnings(writeChar(as.character(object@METADATA[ID,"DATE"]),
                                 con,
                                 nchars = 6,
                                 useBytes=TRUE,
                                 eos = NULL))

      ##SEQUENCE
      ##count number of characters
      SEQUENCE_SIZE <- as.integer(
        nchar(as.character(object@METADATA[["SEQUENCE"]][ID]), type = "bytes", keepNA = FALSE))

      writeBin(SEQUENCE_SIZE,
               con,
               size = 1,
               endian="little")

      writeChar(as.character(object@METADATA[ID,"SEQUENCE"]),
                con,
                nchars = SEQUENCE_SIZE,
                useBytes=TRUE,
                eos = NULL)

      ##stepping
      if(8-SEQUENCE_SIZE>0){
        writeBin(raw(length = (8-SEQUENCE_SIZE)),
                 con,
                 size = 1,
                 endian="little")
      }

      ##USER
      USER_SIZE <- as.integer(nchar(as.character(object@METADATA[ID,"USER"]), type="bytes"))

      writeBin(USER_SIZE,
               con,
               size = 1,
               endian="little")

      writeChar(as.character(object@METADATA[ID,"USER"]),
                con,
                nchars = USER_SIZE,
                useBytes=TRUE,
                eos = NULL)

      ##stepping
      if(8-USER_SIZE>0){
        writeBin(raw(length = (8-USER_SIZE)),
                 con,
                 size = 1,
                 endian="little")
      }

      ##DTYPE
      writeBin(object@METADATA[ID,"DTYPE"],
               con,
               size = 1,
               endian="little")

      ##IRR_TIME
      writeBin(as.double(object@METADATA[ID,"IRR_TIME"]),
               con,
               size = 4,
               endian="little")


      ##IRR_TYPE, IRR_UNIT
      writeBin(c(object@METADATA[ID,"IRR_TYPE"],
                 object@METADATA[ID,"IRR_UNIT"]),
               con,
               size = 1,
               endian="little")


      ##BL_TIME
      writeBin(as.double(object@METADATA[ID,"BL_TIME"]),
               con,
               size = 4,
               endian="little")

      ##BL_UNIT
      writeBin(as.integer(object@METADATA[ID,"BL_UNIT"]),
               con,
               size = 1,
               endian="little")

      ##AN_TEMP, AN_TIME, NORM1, NORM2, NORM2, BG
      writeBin(c(as.double(object@METADATA[ID,"AN_TEMP"]),
                 as.double(object@METADATA[ID,"AN_TIME"]),
                 as.double(object@METADATA[ID,"NORM1"]),
                 as.double(object@METADATA[ID,"NORM2"]),
                 as.double(object@METADATA[ID,"NORM3"]),
                 as.double(object@METADATA[ID,"BG"])),
               con,
               size = 4,
               endian="little")

      ##SHIFT
      writeBin(as.integer(object@METADATA[ID,"SHIFT"]),
               con,
               size = 2,
               endian="little")

      ##SAMPLE
      SAMPLE_SIZE <- as.integer(nchar(as.character(object@METADATA[ID,"SAMPLE"]), type="bytes"))

      ##avoid problems with empty sample names
      if(SAMPLE_SIZE == 0){

        SAMPLE_SIZE <- as.integer(2)
        object@METADATA[ID,"SAMPLE"] <- "  "
      }

      writeBin(SAMPLE_SIZE,
               con,
               size = 1,
               endian="little")

      writeChar(as.character(object@METADATA[ID,"SAMPLE"]),
                con,
                nchars = SAMPLE_SIZE,
                useBytes=TRUE,
                eos = NULL)

      if((20-SAMPLE_SIZE)>0){
        writeBin(raw(length = (20-SAMPLE_SIZE)),
                 con,
                 size = 1,
                 endian="little")
      }

      ##COMMENT
      COMMENT_SIZE <- as.integer(nchar(as.character(object@METADATA[ID,"COMMENT"]), type="bytes"))

      ##avoid problems with empty comments
      if(COMMENT_SIZE == 0){
        COMMENT_SIZE <- as.integer(2)
        object@METADATA[ID,"COMMENT"] <- "  "
      }

      writeBin(COMMENT_SIZE,
               con,
               size = 1,
               endian="little")

      suppressWarnings(writeChar(as.character(object@METADATA[ID,"COMMENT"]),
                                 con,
                                 nchars = COMMENT_SIZE,
                                 useBytes=TRUE,
                                 eos = NULL))


      if((80-COMMENT_SIZE)>0){
        writeBin(raw(length = c(80-COMMENT_SIZE)),
                 con,
                 size = 1,
                 endian="little")
      }

      ##LIGHTSOURCE, SET, TAG
      writeBin(c(as.integer(object@METADATA[ID,"LIGHTSOURCE"]),
                 as.integer(object@METADATA[ID,"SET"]),
                 as.integer(object@METADATA[ID,"TAG"])),
               con,
               size = 1,
               endian="little")

      ##GRAIN
      writeBin(as.integer(object@METADATA[ID,"GRAIN"]),
               con,
               size = 2,
               endian="little")

      ##LPOWER
      writeBin(as.double(object@METADATA[ID,"LPOWER"]),
               con,
               size = 4,
               endian="little")

      ##SYSTEMID
      writeBin(as.integer(object@METADATA[ID,"SYSTEMID"]),
               con,
               size = 2,
               endian="little")

      ##Further distinction needed to fully support format version 03 and 04 separately
      if(version == 03){
        ##RESERVED 1
        if(length(object@.RESERVED) == 0 || version.original != version){
          writeBin(raw(length=36),
                   con,
                   size = 1,
                   endian="little")
        }else{
          writeBin(object = object@.RESERVED[[ID]][[1]],
                   con,
                   size = 1,
                   endian="little")
        }

        ##ONTIME, OFFTIME
        writeBin(c(as.integer(object@METADATA[ID,"ONTIME"]),
                   as.integer(object@METADATA[ID,"OFFTIME"])),
                 con,
                 size = 4,
                 endian="little")

        ##GATE_ENABLED
        writeBin(as.integer(object@METADATA[ID,"GATE_ENABLED"]),
                 con,
                 size = 1,
                 endian="little")


        ##GATE_START, GATE_STOP
        writeBin(c(as.integer(object@METADATA[ID,"GATE_START"]),
                   as.integer(object@METADATA[ID,"GATE_STOP"])),
                 con,
                 size = 4,
                 endian="little")


        ##RESERVED 2
        if(length(object@.RESERVED) == 0 || version.original != version){
          writeBin(raw(length=1),
                   con,
                   size = 1,
                   endian="little")

        }else{
          writeBin(object@.RESERVED[[ID]][[2]],
                   con,
                   size = 1,
                   endian="little")
        }

      } else {
        ##version 04
        ##RESERVED 1
        if(length(object@.RESERVED) == 0 || version.original != version){
          writeBin(raw(length=20),
                   con,
                   size = 1,
                   endian="little")
        } else{
          writeBin(object@.RESERVED[[ID]][[1]],
                   con,
                   size = 1,
                   endian="little")
        }

        ##CURVENO
        writeBin(as.integer(object@METADATA[ID,"CURVENO"]),
                 con,
                 size = 2,
                 endian="little")

        ##TIMETICK
        writeBin(c(as.double(object@METADATA[ID,"TIMETICK"])),
                 con,
                 size = 4,
                 endian="little")

        ##ONTIME, STIMPERIOD
        writeBin(c(as.integer(object@METADATA[ID,"ONTIME"]),
                   as.integer(object@METADATA[ID,"STIMPERIOD"])),
                 con,
                 size = 4,
                 endian="little")

        ##GATE_ENABLED
        writeBin(as.integer(object@METADATA[ID,"GATE_ENABLED"]),
                 con,
                 size = 1,
                 endian="little")

        ##GATE_START, GATE_STOP
        writeBin(c(as.integer(object@METADATA[ID,"GATE_START"]),
                   as.integer(object@METADATA[ID,"GATE_STOP"])),
                 con,
                 size = 4,
                 endian="little")

        ##PTENABLED
        writeBin(as.integer(object@METADATA[ID,"PTENABLED"]),
                 con,
                 size = 1,
                 endian="little")

        ##RESERVED 2
        if(length(object@.RESERVED) == 0 || version.original != version){
          writeBin(raw(length=10),
                   con,
                   size = 1,
                   endian="little")
        } else {
          writeBin(object@.RESERVED[[ID]][[2]],
                   con,
                   size = 1,
                   endian="little")
        }
      }
      ##DPOINTS
      writeBin(as.integer(unlist(object@DATA[ID])),
               con,
               size = 4,
               endian="little")

      #SET UNIQUE ID
      ID <- ID + 1
      ##update progress bar
      if(txtProgressBar) setTxtProgressBar(pb, ID)
    }
  }
  ## ====================================================
  ## version > 06
  if(version == 05 | version == 06 | version == 07 | version == 08){
    ##start loop for export BIN data
    while(ID<=n.records) {
      ##VERSION
      writeBin(as.raw(object@METADATA[ID,"VERSION"]),
               con,
               size = 1,
               endian="little")

      ##stepping
      writeBin(raw(length=1),
               con,
               size = 1,
               endian="little")

      ##LENGTH, PREVIOUS, NPOINTS
      writeBin(c(as.integer(object@METADATA[ID,"LENGTH"]),
                 as.integer(object@METADATA[ID,"PREVIOUS"]),
                 as.integer(object@METADATA[ID,"NPOINTS"])),
               con,
               size = 4,
               endian="little")

      if(version == 08){
        writeBin(as.integer(object@METADATA[ID,"RECTYPE"]),
                 con,
                 size = 1,
                 endian="little")
      }

      ##RUN, SET, POSITION, GRAINNUMBER, CURVENO, XCOORD, YCOORD
      writeBin(c(as.integer(object@METADATA[ID,"RUN"]),
                 as.integer(object@METADATA[ID,"SET"]),
                 as.integer(object@METADATA[ID,"POSITION"]),
                 as.integer(object@METADATA[ID,"GRAINNUMBER"]),
                 as.integer(object@METADATA[ID,"CURVENO"]),
                 as.integer(object@METADATA[ID,"XCOORD"]),
                 as.integer(object@METADATA[ID,"YCOORD"])),
               con,
               size = 2,
               endian="little")

      ##SAMPLE
      SAMPLE_SIZE <- as.integer(nchar(as.character(object@METADATA[ID,"SAMPLE"]), type="bytes"))

      ##avoid problems with empty sample names
      if(SAMPLE_SIZE == 0){

        SAMPLE_SIZE <- as.integer(2)
        object@METADATA[ID,"SAMPLE"] <- "  "
      }

      writeBin(SAMPLE_SIZE,
               con,
               size = 1,
               endian="little")


      writeChar(as.character(object@METADATA[ID,"SAMPLE"]),
                con,
                nchars = SAMPLE_SIZE,
                useBytes=TRUE,
                eos = NULL)

      if((20-SAMPLE_SIZE)>0){
        writeBin(raw(length = (20-SAMPLE_SIZE)),
                 con,
                 size = 1,
                 endian="little")
      }

      ##COMMENT
      COMMENT_SIZE <- as.integer(nchar(as.character(object@METADATA[ID,"COMMENT"]), type="bytes"))

      ##avoid problems with empty comments
      if(COMMENT_SIZE == 0){
        COMMENT_SIZE <- as.integer(2)
        object@METADATA[ID,"COMMENT"] <- "  "
      }

      writeBin(COMMENT_SIZE,
               con,
               size = 1,
               endian="little")

      writeChar(as.character(object@METADATA[ID,"COMMENT"]),
                con,
                nchars = COMMENT_SIZE,
                useBytes=TRUE,
                eos = NULL)

      if((80-COMMENT_SIZE)>0){
        writeBin(raw(length = c(80-COMMENT_SIZE)),
                 con,
                 size = 1,
                 endian="little")
      }

      ##Instrument and sequence characteristics
      ##SYSTEMID
      writeBin(as.integer(object@METADATA[ID,"SYSTEMID"]),
               con,
               size = 2,
               endian="little")

      ##FNAME
      FNAME_SIZE <- as.integer(nchar(as.character(object@METADATA[ID,"FNAME"]), type="bytes"))

        ##correct for case that this is of 0 length
        if(is.na(FNAME_SIZE) || length(FNAME_SIZE) == 0){FNAME_SIZE <- as.integer(0)}

      writeBin(FNAME_SIZE,
               con,
               size = 1,
               endian="little")

      if(FNAME_SIZE>0) {
        writeChar(
          as.character(object@METADATA[ID,"FNAME"]),
          con,
          nchars = FNAME_SIZE,
          useBytes = TRUE,
          eos = NULL
        )
      }

      if((100-FNAME_SIZE)>0){
        writeBin(raw(length = c(100-FNAME_SIZE)),
                 con,
                 size = 1,
                 endian="little")
      }

      ##USER
      USER_SIZE <- as.integer(nchar(as.character(object@METADATA[ID,"USER"]), type="bytes"))

      writeBin(USER_SIZE,
               con,
               size = 1,
               endian="little")

      writeChar(as.character(object@METADATA[ID,"USER"]),
                con,
                nchars = USER_SIZE,
                useBytes=TRUE,
                eos = NULL)

      if((30-USER_SIZE)>0){
        writeBin(raw(length = c(30-USER_SIZE)),
                 con,
                 size = 1,
                 endian="little")
      }

      ##TIME
      TIME_SIZE <- nchar(object@METADATA[ID,"TIME"])

      writeBin(as.integer(TIME_SIZE),
               con,
               size = 1,
               endian="little")

      writeChar(object@METADATA[ID,"TIME"],
                con,
                nchars =TIME_SIZE,
                useBytes=TRUE,
                eos = NULL)

      if(6-TIME_SIZE>0){
        writeBin(raw(length = c(6-TIME_SIZE)),
                 con,
                 size = 1,
                 endian="little")
      }

      ##DATE
      writeBin(as.integer(6),
               con,
               size = 1 ,
               endian="little")


      suppressWarnings(writeChar(as.character(object@METADATA[ID,"DATE"]),
                                 con,
                                 nchars = 6,
                                 useBytes=TRUE,
                                 eos = NULL))

      ##Analysis
      ##DTYPE
      writeBin(object@METADATA[ID,"DTYPE"],
               con,
               size = 1,
               endian="little")

      ##BL_TIME
      writeBin(as.double(object@METADATA[ID,"BL_TIME"]),
               con,
               size = 4,
               endian="little")

      ##BL_UNIT
      writeBin(as.integer(object@METADATA[ID,"BL_UNIT"]),
               con,
               size = 1,
               endian="little")

      ##NORM1, NORM2, NORM3, BG
      writeBin(c(as.double(object@METADATA[ID,"NORM1"]),
                 as.double(object@METADATA[ID,"NORM2"]),
                 as.double(object@METADATA[ID,"NORM3"]),
                 as.double(object@METADATA[ID,"BG"])),
               con,
               size = 4,
               endian="little")

      ##SHIFT
      writeBin(as.integer(object@METADATA[ID,"SHIFT"]),
               con,
               size = 2,
               endian="little")

      ##TAG
      writeBin(c(as.integer(object@METADATA[ID,"TAG"])),
               con,
               size = 1,
               endian="little")

      ##RESERVED 1
      if(length(object@.RESERVED) == 0 || version.original != version){
        writeBin(raw(length=20),
                 con,
                 size = 1,
                 endian="little")
      }else{
        writeBin(object@.RESERVED[[ID]][[1]],
                 con,
                 size = 1,
                 endian="little")
      }

      ##Measurement characteristics
      ##LTYPE
      writeBin(object@METADATA[ID,"LTYPE"],
               con,
               size = 1,
               endian="little")

      ##LIGHTSOURCE
      writeBin(c(as.integer(object@METADATA[ID,"LIGHTSOURCE"])),
               con,
               size = 1,
               endian="little")

      ##LIGHTPOWER, LOW, HIGH, RATE
      writeBin(c(as.double(object@METADATA[ID,"LIGHTPOWER"]),
                 as.double(object@METADATA[ID,"LOW"]),
                 as.double(object@METADATA[ID,"HIGH"]),
                 as.double(object@METADATA[ID,"RATE"])),
               con,
               size = 4,
               endian="little")

      ##TEMPERATURE, MEASTEMP
      writeBin(c(as.integer(object@METADATA[ID,"TEMPERATURE"]),
                 as.integer(object@METADATA[ID,"MEASTEMP"])),
               con,
               size = 2,
               endian="little")

      ##AN_TEMP, AN_TIME
      writeBin(c(as.double(object@METADATA[ID,"AN_TEMP"]),
                 as.double(object@METADATA[ID,"AN_TIME"])),
               con,
               size = 4,
               endian="little")

      ##TOLDELAY; TOLON, TOLOFF
      writeBin(c(as.integer(object@METADATA[ID,"TOLDELAY"]),
                 as.integer(object@METADATA[ID,"TOLON"]),
                 as.integer(object@METADATA[ID,"TOLOFF"])),
               con,
               size = 2,
               endian="little")

      ##IRR_TIME
      writeBin(as.double(object@METADATA[ID,"IRR_TIME"]),
               con,
               size = 4,
               endian="little")

      ##IRR_TYPE
      writeBin(c(object@METADATA[ID,"IRR_TYPE"]),
               con,
               size = 1,
               endian="little")

      ##IRR_DOSERATE, IRR_DOSERATEERR
      if(version == 05){
        writeBin(as.double(object@METADATA[ID,"IRR_DOSERATE"]),
                 con,
                 size = 4,
                 endian="little")
      }else{
        writeBin(c(as.double(object@METADATA[ID,"IRR_DOSERATE"]),
                   as.double(object@METADATA[ID,"IRR_DOSERATEERR"])),
                 con,
                 size = 4,
                 endian="little")
      }

      ##TIMESINCEIRR
      writeBin(c(as.integer(object@METADATA[ID,"TIMESINCEIRR"])),
               con,
               size = 4,
               endian="little")

      ##TIMETICK
      writeBin(c(as.double(object@METADATA[ID,"TIMETICK"])),
               con,
               size = 4,
               endian="little")

      ##ONTIME, STIMPERIOD
      writeBin(c(suppressWarnings(as.integer(object@METADATA[ID,"ONTIME"])),
                 as.integer(object@METADATA[ID,"STIMPERIOD"])),
               con,
               size = 4,
               endian="little")

      ##GATE_ENABLED
      writeBin(as.integer(object@METADATA[ID,"GATE_ENABLED"]),
               con,
               size = 1,
               endian="little")

      ##GATE_START, GATE_STOP
      writeBin(c(as.integer(object@METADATA[ID,"GATE_START"]),
                 as.integer(object@METADATA[ID,"GATE_STOP"])),
               con,
               size = 4,
               endian="little")

      ##PTENABLED, DTENABLED
      writeBin(c(as.integer(object@METADATA[ID,"PTENABLED"]),
                 as.integer(object@METADATA[ID,"DTENABLED"])),
               con,
               size = 1,
               endian="little")

      ##DEADTIME, MAXLPOWER, XRF_ACQTIME, XRF_HV
      writeBin(c(as.double(object@METADATA[ID,"DEADTIME"]),
                 as.double(object@METADATA[ID,"MAXLPOWER"]),
                 as.double(object@METADATA[ID,"XRF_ACQTIME"]),
                 as.double(object@METADATA[ID,"XRF_HV"])),
               con,
               size = 4,
               endian="little")

      ##XRF_CURR
      writeBin(c(as.integer(object@METADATA[ID,"XRF_CURR"])),
               con,
               size = 4,
               endian="little")

      ##XRF_DEADTIMEF
      writeBin(c(as.double(object@METADATA[ID,"XRF_DEADTIMEF"])),
               con,
               size = 4,
               endian="little")

      ##add version support for V7
      if(version == 05){
        ##RESERVED 2
        if(length(object@.RESERVED) == 0 || version.original != version){
          writeBin(raw(length=4),
                   con,
                   size = 1,
                   endian="little")
        }else{
          writeBin(object@.RESERVED[[ID]][[2]],
                   con,
                   size = 1,
                   endian="little")
        }

      }else if(version == 06){

        ##RESERVED 2
        if(length(object@.RESERVED) == 0 || version.original != version){
          writeBin(raw(length=24),
                   con,
                   size = 1,
                   endian="little")
        }else{
          writeBin(object@.RESERVED[[ID]][[2]],
                   con,
                   size = 1,
                   endian="little")
        }

      }else{
        ##DETECTOR_ID
        writeBin(as.integer(object@METADATA[ID,"DETECTOR_ID"]),
                 con,
                 size = 1,
                 endian="little")

        ##LOWERFILTER_ID, UPPERFILTER_ID
        writeBin(c(as.integer(object@METADATA[ID,"LOWERFILTER_ID"]),
                   as.integer(object@METADATA[ID,"UPPERFILTER_ID"])),
                 con,
                 size = 2,
                 endian="little")

        ##ENOISEFACTOR
        writeBin(as.double(object@METADATA[ID,"ENOISEFACTOR"]),
                 con,
                 size = 4,
                 endian="little")


        ##VERSION 08
        if(version == 07){

          ##RESERVED 2
          if(length(object@.RESERVED) == 0 || version.original != version){
            writeBin(raw(length=15),
                     con,
                     size = 1,
                     endian="little")
          }else{
            writeBin(object@.RESERVED[[ID]][[2]],
                     con,
                     size = 1,
                     endian="little")
          }

        }else{

          ##MARKPOS POSITION and extraction
          writeBin(
            c(
              as.double(object@METADATA[ID, "MARKPOS_X1"]),
              as.double(object@METADATA[ID, "MARKPOS_Y1"]),
              as.double(object@METADATA[ID, "MARKPOS_X2"]),
              as.double(object@METADATA[ID, "MARKPOS_Y2"]),
              as.double(object@METADATA[ID, "MARKPOS_X3"]),
              as.double(object@METADATA[ID, "MARKPOS_Y3"]),
              as.double(object@METADATA[ID, "EXTR_START"]),
              as.double(object@METADATA[ID, "EXTR_END"])
            ),
            con,
            size = 4,
            endian = "little"
          )

          ##RESERVED 2
          if(length(object@.RESERVED) == 0 || version.original != version){
            writeBin(raw(length=42),
                     con,
                     size = 1,
                     endian="little")
          }else{
            writeBin(object@.RESERVED[[ID]][[2]],
                     con,
                     size = 1,
                     endian="little")
          }
        }

      }#end if version decision
      ##DPOINTS
      writeBin(as.integer(unlist(object@DATA[ID])),
               con,
               size = 4,
               endian="little")

      #SET UNIQUE ID
      ID <- ID + 1

      ##update progress bar
      if(txtProgressBar)  setTxtProgressBar(pb, ID)
    }
  }

  # ##close
  if(txtProgressBar) close(pb)

  ##output
  if (verbose)
    message("\t >> ", ID - 1, " records written successfully\n")

  ## return path
  invisible(file)
}

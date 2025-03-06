#' @title Import Risø BIN/BINX-files into R
#'
#' @description Import a `*.bin` or a `*.binx` file produced by a Risø DA15 and DA20 TL/OSL
#' reader into R.
#'
#' @details
#'
#' The binary data file is parsed byte by byte following the data structure
#' published in the Appendices of the Analyst manual p. 42.
#'
#' For the general BIN/BINX-file structure, the reader is referred to the
#' Risø website: [https://www.fysik.dtu.dk]()
#'
#' @param file [character] or [list] (**required**): path and file name of the
#' BIN/BINX file (URLs are supported). If input is a `list` it should comprise
#' only `character`s representing each valid path and BIN/BINX-file names.
#' Alternatively, the input character can be just a directory (path), in which
#' case the function tries to detect and import all BIN/BINX files found in
#' the directory.
#'
#' @param show.raw.values [logical] (*with default*):
#' shows raw values from BIN-file for `LTYPE`, `DTYPE` and `LIGHTSOURCE` without
#' translation in characters. Can be provided as `list` if `file` is a `list`.
#'
#' @param n.records [numeric] (*optional*): limits the number of imported records
#' to the provided record id (e.g., `n.records = 1:10` imports the first ten records,
#' while `n.records = 3` imports only record number 3. Can be used in combination with
#' `show.record.number` for debugging purposes, e.g. corrupt BIN-files.
#' Can be provided as `list` if `file` is a `list`.
#'
#' @param zero_data.rm [logical] (*with default*):
#' remove erroneous data with no count values. As such data are usually not
#' needed for the subsequent data analysis they will be removed by default.
#' Can be provided as `list` if `file` is a `list`.
#'
#' @param duplicated.rm [logical] (*with default*):
#' remove duplicated entries if `TRUE`. This may happen due to an erroneous
#' produced BIN/BINX-file. This option compares only predecessor and successor.
#' Can be provided as `list` if `file` is a `list`.
#'
#' @param position [numeric] (*optional*):
#' imports only the selected position. Note: the import performance will not
#' benefit by any selection made here.
#' Can be provided as `list` if `file` is a `list`.
#'
#' @param fastForward [logical] (*with default*):
#' if `TRUE` for a more efficient data processing only a list of `RLum.Analysis`
#' objects is returned instead of a [Risoe.BINfileData-class] object.
#' Can be provided as `list` if `file` is a `list`.
#'
#' @param show.record.number [logical] (*with default*):
#' shows record number of the imported record, for debugging usage only.
#' Can be provided as `list` if `file` is a `list`.
#' Ignored if `verbose = FALSE`.
#'
#' @param txtProgressBar [logical] (*with default*):
#' enable/disable the progress bar. Ignored if `verbose = FALSE`.
#'
#' @param forced.VersionNumber [integer] (*optional*):
#' allows to cheat the version number check in the function by own values for
#' cases where the BIN-file version is not supported.
#' Can be provided as `list` if `file` is a `list`.
#'
#' **Note:** The usage is at own risk, only supported BIN-file versions have been tested.
#'
#' @param ignore.RECTYPE [logical] or [numeric] (*with default*):
#' this argument allows to ignore values in the byte 'RECTYPE' (BIN-file version 08),
#' in case there are not documented or faulty set. In this case the corrupted records are skipped.
#' If the setting is [numeric] (e.g., `ignore.RECTYPE = 128`), records of those type are ignored
#' for import.
#'
#' @param pattern [character] (*optional*):
#' argument that is used if only a path is provided. The argument will than be
#' passed to the function [list.files] used internally to construct a `list`
#' of wanted files
#'
#' @param verbose [logical] (*with default*):
#' enable/disable output to the terminal.
#'
#' @param ... further arguments that will be passed to the function
#' [Risoe.BINfileData2RLum.Analysis]. Please note that any matching argument
#' automatically sets `fastForward = TRUE`
#'
#' @return
#' Returns an S4 [Risoe.BINfileData-class] object containing two
#' slots:
#'
#' \item{METADATA}{A [data.frame] containing all variables stored in the BIN-file.}
#' \item{DATA}{A [list] containing a numeric [vector] of the measured data.
#' The ID corresponds to the record ID in METADATA.}
#'
#' If `fastForward = TRUE` a list of [RLum.Analysis-class] object is returned. The
#' internal coercing is done using the function [Risoe.BINfileData2RLum.Analysis]
#'
#' @note
#' The function works for BIN/BINX-format versions 03, 04, 05, 06, 07 and 08. The
#' version number depends on the used Sequence Editor.
#'
#' @section Function version: 0.18
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)\cr
#' Margret C. Fuchs, HZDR Freiberg, (Germany) \cr
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)\cr
#' based on information provided by Torben Lapp and Karsten Bracht Nielsen (Risø DTU, Denmark)
#'
#'
#' @seealso [write_R2BIN], [Risoe.BINfileData-class],
#' [base::readBin], [merge_Risoe.BINfileData], [RLum.Analysis-class]
#' [utils::txtProgressBar], [list.files]
#'
#'
#'@references
#'DTU Nutech, 2016. The Sequence Editor, Users Manual, February, 2016.
#'[https://www.fysik.dtu.dk]()
#'
#'
#'@keywords IO
#'
#'@examples
#'
#'file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
#'temp <- read_BIN2R(file)
#'temp
#'
#' @md
#' @export
read_BIN2R <- function(
  file,
  show.raw.values = FALSE,
  position = NULL,
  n.records = NULL,
  zero_data.rm = TRUE,
  duplicated.rm = FALSE,
  fastForward = FALSE,
  show.record.number = FALSE,
  txtProgressBar = TRUE,
  forced.VersionNumber = NULL,
  ignore.RECTYPE = FALSE,
  pattern = NULL,
  verbose = TRUE,
  ...
) {
  .set_function_name("read_BIN2R")
  on.exit(.unset_function_name(), add = TRUE)

  # Self Call -----------------------------------------------------------------------------------
  # Option (a): Input is a list, every element in the list will be treated as file connection
  # with that many file can be read in at the same time
  # Option (b): The input is just a path, the function tries to grep ALL BIN/BINX files in the
  # directory and import them, if this is detected, we proceed as list
  .validate_class(file, c("character", "list"))

  if (is.character(file)) {
    .validate_not_empty(file)

    if (is.null(pattern)) {
      ##If this is not really a path we skip this here
      if (all(dir.exists(file)) & length(dir(file)) > 0) {
        if (verbose)
          message("[read_BIN2R()] Directory detected, trying to extract ",
                  "'*.bin'/'*.binx' files ...\n")

        ##get files
        file <- as.list(list.files(
          path = file,
          recursive = FALSE,
          pattern = "\\.bin*",
          full.names = TRUE,
          ignore.case = TRUE))
      }

    }else if(dir.exists(file)){
      file <- as.list(list.files(file, pattern = pattern, full.names = TRUE, recursive = TRUE))
    }
  }

  if (is.list(file)) {
    ## expand input arguments
    rep.length <- length(file)

    position <- .listify(position, rep.length)
    n.records <- .listify(n.records, rep.length)
    zero_data.rm <- .listify(zero_data.rm, rep.length)
    duplicated.rm <- .listify(duplicated.rm, rep.length)
    show.raw.values <- .listify(show.raw.values, rep.length)
    show.record.number <- .listify(show.record.number, rep.length)
    forced.VersionNumber <- .listify(forced.VersionNumber, rep.length)

    temp.return <- lapply(seq_along(file), function(x) {
      temp <- read_BIN2R(
        file = file[[x]],
        fastForward = fastForward,
        position = position[[x]],
        n.records = n.records[[x]],
        duplicated.rm = duplicated.rm[[x]],
        zero_data.rm = zero_data.rm[[x]],
        show.raw.values =  show.raw.values[[x]],
        show.record.number = show.record.number[[x]],
        txtProgressBar = txtProgressBar,
        forced.VersionNumber = forced.VersionNumber[[x]],
        ignore.RECTYPE = ignore.RECTYPE,
        verbose = verbose,
        ...
      )
    })

    ##return
    if (fastForward) {
      return(unlist(temp.return, recursive = FALSE))

    }else{
      return(temp.return)
    }
  }

  ## Config -----------------------------------------------------------------

  .validate_length(file, 1)
  .validate_logical_scalar(show.raw.values)
  .validate_logical_scalar(zero_data.rm)
  .validate_logical_scalar(duplicated.rm)
  .validate_logical_scalar(fastForward)
  .validate_logical_scalar(show.record.number)
  .validate_logical_scalar(txtProgressBar)
  .validate_class(ignore.RECTYPE, c("logical", "numeric"))

  ##set file_link for internet downloads
  url_file <- NULL
  on_exit <- function(){
    ##unlink internet connection
    if(!is.null(url_file)){
      unlink(url_file)
    }

    ##close connection
    if(exists("con") && !is.null(con)){
      close(con)
    }
  }
  on.exit(expr = on_exit(), add = TRUE)

  ## never show the progress bar if not verbose
  if (!verbose) {
    txtProgressBar <- FALSE
  }

  ## check for URL and attempt download
  url_file <- .download_file(file, verbose = verbose,
                             tempfile("read_BIN22R_FILE", fileext = ".binx"))

  if(!is.null(url_file))
    file <- url_file

  ## normalise path, just in case
  file <- suppressWarnings(normalizePath(file))

  ## check whether file exists
  info <- file.info(file)
  if (is.na(info$size)) {
    .throw_error("File '", file, "' does not exist")
  }

  ## skip if zero-byte
  if (info$size == 0) {
    .throw_message("File '", file, "' is a zero-byte file, NULL returned")
    return(NULL)
  }

  ## check if file is a BIN or BINX file
  if(!any(tolower(tools::file_ext(file)) %in%  c("bin", "binx"))) {
    .throw_message("File '", file, "' is not a file of type ",
                   "'BIN' or 'BINX', NULL returned")
    con <- NULL
    return(NULL)
  }

  ##set supported BIN format version
  VERSIONS.supported <- as.raw(c(03, 04, 05, 06, 07, 08))

  ## Short file parsing to get number of records ----------------------------

  #open connection
  con <- file(file, "rb")

  ##read data up to the end of con
  ##set ID
  temp.ID <- 0

  ##start for BIN-file check up
  while(length(temp.VERSION <- readBin(con, what="raw", 1, size=1, endian="little"))>0) {
    ## force version number
    if(!is.null(forced.VersionNumber)){
      temp.VERSION <- as.raw(forced.VersionNumber)
      if (verbose)
        message("[read_BIN2R()] 'forced.VersionNumber' set to ", temp.VERSION,
                ", but this version may not match your input file")
    }

    ##stop input if wrong VERSION
    if (!temp.VERSION %in% VERSIONS.supported) {
      if(temp.ID > 0){
        if(is.null(n.records)){
          .throw_warning("BIN-file appears to be corrupt, import limited ",
                         "to the first ", temp.ID, " records")
        }else{
          .throw_warning("BIN-file appears to be corrupt, 'n.records' ",
                         "reset to ", temp.ID)
        }

        ##set or reset n.records
        n.records <- seq_len(temp.ID)
        break()

      }else{
        .throw_error("BIN/BINX format version (", temp.VERSION, ") ",
                     "is not supported or file is broken. ",
                     "Supported version numbers are: ",
                     .collapse(VERSIONS.supported))
      }
    }

    #empty byte position
    seek.connection(con, 1, origin = "current")

    ## get record LENGTH
    int.size <- if (temp.VERSION >= 05) 4 else 2
    temp.LENGTH  <- readBin(con, what = "integer", 1, size = int.size,
                            endian = "little")

    num.toread <- max(0, temp.LENGTH - int.size - 2)
    if (num.toread > 0) {
      seek.connection(con, num.toread, origin = "current")
    } else {
      if (verbose)
        message("\n[read_BIN2R()] Record #", temp.ID + 1,
                " skipped due to wrong record length")
      next()
    }
    temp.ID <- temp.ID + 1
  }

  ##set n.length we will need it later
  n.length <- temp.ID
  if (n.length == 0) {
    .throw_warning("0 records read, NULL returned")
    return(NULL)
  }

  rm(temp.ID)
  close(con) ##we have to close the connection here

# Set Lookup tables  --------------------------------------------------------------------------

  ##LTYPE
  LTYPE.lookup <- c(
    "0" = "TL",
    "1" = "OSL",
    "2" = "IRSL",
    "3" = "M-IR",
    "4" = "M-VIS",
    "5" = "TOL",
    "6" = "TRPOSL",
    "7" = "RIR",
    "8" = "RBR",
    "9" = "USER",
    "10" = "POSL",
    "11" = "SGOSL",
    "12" = "RL",
    "13" = "XRF"
  )

  ##DTYPE
  DTYPE.lookup <-
    c(
      "0" = "Natural",
      "1" = "N+dose",
      "2" = "Bleach",
      "3" = "Bleach+dose",
      "4" = "Natural (Bleach)",
      "5" = "N+dose (Bleach)",
      "6" = "Dose",
      "7" = "Background"
    )

  ##LIGHTSOURCE
  LIGHTSOURCE.lookup <- c(
    "0" = "None",
    "1" = "Lamp",
    "2" = "IR diodes/IR Laser",
    "3" = "Calibration LED",
    "4" = "Blue Diodes",
    "5" = "White light",
    "6" = "Green laser (single grain)",
    "7" = "IR laser (single grain)"
  )

  ## helper to import string from pascal format
  .read_string <- function(con, field.length, force.size = NULL) {
    raw <- readBin(con, what = "raw", field.length, size = 1, endian = "little")
    strlen <- max(as.integer(raw[1]), 0)
    if (strlen > 0) {
      if (!is.null(force.size))
        strlen <- force.size
      return(suppressWarnings(readChar(raw[-1], strlen, useBytes = TRUE)))
    }
    return("")
  }

  ##PRESET VALUES
  temp.RECTYPE <- 0

  ##overwrite length if required
  if(!is.null(n.records))
    n.length <- length(n.records)

  ## set index for entry row in table
  id_row <- 1

  ## 1 to 7
  ID       <- integer(length = n.length)
  SEL      <- NULL # derived from TAG
  VERSION  <- rep_len(NA_integer_, n.length)
  LENGTH   <- integer(length = n.length)
  PREVIOUS <- integer(length = n.length)
  NPOINTS  <- integer(length = n.length)
  RECTYPE  <- integer(length = n.length)

  ## 8 to 17
  RUN         <- rep_len(NA_integer_, n.length)
  SET         <- rep_len(NA_integer_, n.length)
  POSITION    <- integer(n.length) # default value 0
  GRAIN       <- NULL # derived from GRAINNUMBER
  GRAINNUMBER <- integer(n.length) # default value 0
  CURVENO     <- rep_len(NA_integer_, n.length)
  XCOORD      <- rep_len(NA_integer_, n.length)
  YCOORD      <- rep_len(NA_integer_, n.length)
  SAMPLE      <- character(length = n.length)
  COMMENT     <- character(length = n.length)

  ## 18 to 22
  SYSTEMID <- rep_len(NA_integer_, n.length)
  FNAME    <- character(length = n.length)
  USER     <- character(length = n.length)
  TIME     <- character(length = n.length)
  DATE     <- character(length = n.length)

  ## 23 to 31
  DTYPE   <- character(length = n.length)
  BL_TIME <- rep_len(NA_real_, n.length)
  BL_UNIT <- rep_len(NA_integer_, n.length)
  NORM1   <- rep_len(NA_real_, n.length)
  NORM2   <- rep_len(NA_real_, n.length)
  NORM3   <- rep_len(NA_real_, n.length)
  BG      <- rep_len(NA_real_, n.length)
  SHIFT   <- rep_len(NA_integer_, n.length)
  TAG     <- rep_len(NA_integer_, n.length)

  ## 32 to 67
  LTYPE        <- character(length = n.length)
  LIGHTSOURCE  <- character(length = n.length)
  LPOWER       <- NULL # derived from LIGHTPOWER
  LIGHTPOWER   <- rep_len(NA_real_, n.length)
  LOW          <- rep_len(NA_real_, n.length)
  HIGH         <- rep_len(NA_real_, n.length)
  RATE         <- rep_len(NA_real_, n.length)
  TEMPERATURE  <- rep_len(NA_real_, n.length)
  MEASTEMP     <- rep_len(NA_real_, n.length)
  AN_TEMP      <- rep_len(NA_real_, n.length)
  AN_TIME      <- rep_len(NA_real_, n.length)
  TOLDELAY     <- rep_len(NA_integer_, n.length)
  TOLON        <- rep_len(NA_integer_, n.length)
  TOLOFF       <- rep_len(NA_integer_, n.length)
  IRR_TIME     <- rep_len(NA_real_, n.length)
  IRR_TYPE     <- rep_len(NA_integer_, n.length)
  IRR_UNIT     <- rep_len(NA_integer_, n.length)
  IRR_DOSERATE <- rep_len(NA_real_, n.length)
  IRR_DOSERATEERR <- rep_len(NA_real_, n.length)
  TIMESINCEIRR  <- rep_len(NA_real_, n.length)
  TIMETICK      <- rep_len(NA_real_, n.length)
  ONTIME        <- rep_len(NA_real_, n.length)
  OFFTIME       <- rep_len(NA_real_, n.length)
  STIMPERIOD    <- rep_len(NA_integer_, n.length)
  GATE_ENABLED  <- rep_len(NA_real_, n.length)
  ENABLE_FLAGS  <- NULL # derived from GATE_ENABLED
  GATE_START    <- rep_len(NA_real_, n.length)
  GATE_STOP     <- rep_len(NA_real_, n.length)
  PTENABLED     <- rep_len(NA_real_, n.length)
  DTENABLED     <- rep_len(NA_real_, n.length)
  DEADTIME      <- rep_len(NA_real_, n.length)
  MAXLPOWER     <- rep_len(NA_real_, n.length)
  XRF_ACQTIME   <- rep_len(NA_real_, n.length)
  XRF_HV        <- rep_len(NA_real_, n.length)
  XRF_CURR      <- rep_len(NA_real_, n.length)
  XRF_DEADTIMEF <- rep_len(NA_real_, n.length)

  ## 68 to 79
  DETECTOR_ID    <- rep_len(NA_integer_, n.length)
  LOWERFILTER_ID <- rep_len(NA_integer_, n.length)
  UPPERFILTER_ID <- rep_len(NA_integer_, n.length)
  ENOISEFACTOR   <- rep_len(NA_real_, n.length)
  MARKPOS_X1 <- MARKPOS_Y1 <- rep_len(NA_real_, n.length)
  MARKPOS_X2 <- MARKPOS_Y2 <- rep_len(NA_real_, n.length)
  MARKPOS_X3 <- MARKPOS_Y3 <- rep_len(NA_real_, n.length)
  EXTR_START <- rep_len(NA_real_, n.length)
  EXTR_END   <- rep_len(NA_real_, n.length)

  ## 80
  SEQUENCE <- character(length = n.length)

  #set variable for DPOINTS handling
  results.DATA <- list()

  ##set list for RESERVED values
  results.RESERVED <- rep(list(list()), n.length)

  # Open Connection ---------------------------------------------------------

  #open connection
  con <- file(file, "rb")

  if (verbose) {
    cat("\n[read_BIN2R()] Importing ...")
    cat("\n path: ", dirname(file))
    cat("\n file: ", .shorten_filename(basename(file)))
    cat("\n n_rec:", n.length)
    cat("\n")
  }

  ##set progress bar
  if (txtProgressBar) {
    pb <- txtProgressBar(min = 0, max = info$size, char = "=", style = 3)
  }

  ##read data up to the end of con

  ##set ID
  temp.ID <- 0

  # LOOP --------------------------------------------------------------------
  ##start loop for import BIN data
  while(length(temp.VERSION <- readBin(con, what="raw", 1, size=1, endian="little"))>0) {

    ##force version number
    if(!is.null(forced.VersionNumber)){
      temp.VERSION <- as.raw(forced.VersionNumber)
    }

    ##print record ID for debugging purposes
    if(verbose){
      if(show.record.number == TRUE){
        cat(temp.ID,",", sep = "")
        if(temp.ID%%10==0){
          cat("\n")
        }
      }
    }

    #empty byte position
    seek.connection(con, 1, origin = "current")

    ## (1) Header size and structure
    ## LENGTH, PREVIOUS, NPOINTS
    int.size <- if (temp.VERSION >= 05) 4 else 2
    temp <- readBin(con, what = "int", 3, size = int.size, endian = "little")
    temp.LENGTH   <- temp[1]
    temp.PREVIOUS <- temp[2]
    temp.NPOINTS  <- temp[3]

    ## skip record if not selected in n.records
    ## the first condition boosts the speed of reading if n.records is not used
    if (!is.null(n.records) && !(temp.ID + 1) %in% n.records) {
      temp.ID <- temp.ID + 1
      seek.connection(con, temp.LENGTH - 3 * int.size - 2, origin = "current")
      next()
    }

    ## these must be set only after the n.records check
    ## we don't set VERSION now, as we must leave it set to NA in case we
    ## decide to skip the current record
    LENGTH[id_row]   <- temp.LENGTH
    PREVIOUS[id_row] <- temp.PREVIOUS
    NPOINTS[id_row]  <- temp.NPOINTS

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## BINX FORMAT SUPPORT
    if (temp.VERSION == 05 || temp.VERSION == 06 ||
        temp.VERSION == 07 || temp.VERSION == 08) {

      #for temp.VERSION == 08
      #RECTYPE
      if(temp.VERSION == 08){
        temp.RECTYPE <- readBin(con, what = "int", 1, size = 1, endian = "little", signed = FALSE)
        RECTYPE[id_row] <- temp.RECTYPE

        ## we can check for a specific value for temp.RECTYPE
        if(inherits(ignore.RECTYPE[1], "numeric") && temp.RECTYPE == ignore.RECTYPE[1]) {
          seek.connection(con, temp.LENGTH - 15, origin = "current")
            if(verbose)
              message("\n[read_BIN2R()] Record #", temp.ID + 1,
                      " skipped due to ignore.RECTYPE setting")
            next()
          }

        if(temp.RECTYPE != 0 & temp.RECTYPE != 1 & temp.RECTYPE != 128) {
          ##jump to the next record by stepping the record length minus the already read bytes
          seek.connection(con, temp.LENGTH - 15, origin = "current")
          msg <- paste0("Byte RECTYPE = ", temp.RECTYPE,
                        " is not supported in record #", temp.ID + 1)
          if (!ignore.RECTYPE) {
            .throw_error(msg, ", set `ignore.RECTYPE = TRUE` to skip this record")
          }

          ## skip to next record
          if (verbose)
            message("\n[read_BIN2R()] ", msg, ", record skipped")
          temp.ID <- temp.ID + 1
          next()
        }
      }

      ## RECTYPE == 128
      ## If the RECTYPE is 128, only the header bytes until here make any sense,
      ## the rest are just random bytes (e-mail K.B., 2024-07-04)
      ## the header length is 507, hence we have to jump 507 - 15 to get
      ## the data
      ## This is a very ugly construction and the function should be refactored
      if (temp.RECTYPE == 128){
        seek.connection(con, 492, origin = "current")

      } else {
        ##(2) Sample characteristics
        ##RUN, SET, POSITION, GRAINNUMBER, CURVENO, XCOORD, YCOORD
        temp <- readBin(con, what = "int", 7, size = 2, endian = "little")
        RUN[id_row] <- temp[1]
        SET[id_row] <- temp[2]
        POSITION[id_row] <- temp[3]
        GRAINNUMBER[id_row] <- temp[4]
        CURVENO[id_row] <- temp[5]
        XCOORD[id_row] <- temp[6]
        YCOORD[id_row] <- temp[7]

        ##SAMPLE, COMMENT
        SAMPLE[id_row]  <- .read_string(con, 21)
        COMMENT[id_row] <- .read_string(con, 81)

        ##(3) Instrument and sequence characteristic
        ##SYSTEMID
        SYSTEMID[id_row] <- readBin(con, what = "integer", 1, size = 2, endian = "little")

        ## FNAME, USER, TIME, DATE
        FNAME[id_row] <- .read_string(con, 101)
        USER[id_row]  <- .read_string(con, 31)
        TIME[id_row]  <- .read_string(con, 7)
        DATE[id_row]  <- .read_string(con, 7, force.size = 6)

        ##(4) Analysis
        ##DTYPE
        DTYPE[id_row] <- readBin(con, what="int", 1, size=1, endian="little")

        ##BL_TIME
        BL_TIME[id_row] <- readBin(con, what="double", 1, size=4, endian="little")

        ##BL_UNIT
        BL_UNIT[id_row] <- readBin(con, what="int", 1, size=1, endian="little")

        ##NORM1, NORM2, NORM3, BG
        temp <- readBin(con, what="double", 4, size=4, endian="little")
        NORM1[id_row] <- temp[1]
        NORM2[id_row] <- temp[2]
        NORM3[id_row] <- temp[3]
        BG[id_row] <- temp[4]

        ##SHIFT
        SHIFT[id_row] <- readBin(con, what = "int", 1, size = 2, endian = "little")

        ##TAG
        TAG[id_row] <- readBin(con, what = "int", 1, size = 1, endian = "little")

        ##RESERVED
        temp.RESERVED1 <-readBin(con, what="raw", 20, size=1, endian="little")

        ##(5) Measurement characteristics

        ##LTYPE
        ##LTYPESOURCE
        temp <- readBin(con, what = "integer", 2, size = 1, endian = "little")
        LTYPE[id_row] <- temp[1]
        LIGHTSOURCE[id_row] <- temp[2]

        ##LIGHTPOWER, LOW, HIGH, RATE
        temp <- readBin(con, what="double", 4, size=4, endian="little")
        LIGHTPOWER[id_row] <- temp[1]
        LOW[id_row] <- temp[2]
        HIGH[id_row] <- temp[3]
        RATE[id_row] <- temp[4]

        ##TEMPERATURE
        ##MEASTEMP
        temp <- readBin(con, what = "integer", 2, size = 2, endian = "little")
        TEMPERATURE[id_row] <- temp[1]
        MEASTEMP[id_row] <- temp[2]

        ##AN_TEMP
        ##AN_TIME
        temp <- readBin(con, what = "double", 2, size = 4, endian = "little")
        AN_TEMP[id_row] <- temp[1]
        AN_TIME[id_row] <- temp[2]

        ##DELAY, ON, OFF
        temp <- readBin(con, what="int", 3, size=2, endian="little")
        TOLDELAY[id_row] <- temp[1]
        TOLON[id_row] <- temp[2]
        TOLOFF[id_row] <- temp[3]

        ##IRR_TIME
        IRR_TIME[id_row] <- readBin(con, what="double", 1, size=4, endian="little")

        ##IRR_TYPE
        IRR_TYPE[id_row] <- readBin(con, what="int", 1, size=1, endian="little")

        ##IRR_DOSERATE
        IRR_DOSERATE[id_row] <- readBin(con, what="double", 1, size=4, endian="little")

        ##IRR_DOSERATEERR
        if(temp.VERSION != 05)
          IRR_DOSERATEERR[id_row] <- readBin(con, what="double", 1, size=4, endian="little")

        ##TIMESINCEIRR
        TIMESINCEIRR[id_row] <- readBin(con, what="integer", 1, size=4, endian="little")

        ##TIMETICK
        TIMETICK[id_row] <- readBin(con, what="double", 1, size=4, endian="little")

        ##ONTIME
        ##STIMPERIOD
        temp <- readBin(con, what = "integer", 2, size = 4, endian = "little")
        ONTIME[id_row] <- temp[1]
        STIMPERIOD[id_row] <- temp[2]

        ##GATE_ENABLED
        GATE_ENABLED[id_row] <- as.numeric(readBin(con, what="raw", 1, size=1, endian="little"))

        ##GATE_START
        ##GATE_STOP
        temp <- readBin(con, what = "integer", 2, size = 4, endian = "little")
        GATE_START[id_row] <- temp[1]
        GATE_STOP[id_row] <- temp[2]

        ##PTENABLED
        ##DTENABLED
        temp <- as.numeric(readBin(con, what = "raw", 2, size = 1, endian = "little"))
        PTENABLED[id_row] <- temp[1]
        DTENABLED[id_row] <- temp[2]

        ##DEADTIME, MAXLPOWER, XRF_ACQTIME, XRF_HV
        temp <- readBin(con, what="double", 4, size=4, endian="little")
        DEADTIME[id_row] <- temp[1]
        MAXLPOWER[id_row] <- temp[2]
        XRF_ACQTIME[id_row] <- temp[3]
        XRF_HV[id_row] <- temp[4]

        ##XRF_CURR
        XRF_CURR[id_row] <- readBin(con, what="integer", 1, size=4, endian="little")

        ##XRF_DEADTIMEF
        XRF_DEADTIMEF[id_row] <- readBin(con, what="double", 1, size=4, endian="little")

        ###Account for differences between V5, V6 and V7
        if(temp.VERSION == 06){
          reserved2.len <- 24

        }else if(temp.VERSION == 05){
          reserved2.len <- 4

        }else{

          ##DETECTOR_ID
          DETECTOR_ID[id_row] <- readBin(con, what="int", 1, size=1, endian="little")

          ##LOWERFILTER_ID, UPPERFILTER_ID
          temp <- readBin(con, what="int", 2, size=2, endian="little")
          LOWERFILTER_ID[id_row] <- temp[1]
          UPPERFILTER_ID[id_row] <- temp[2]

          ##ENOISEFACTOR
          ENOISEFACTOR[id_row] <- readBin(con, what="double", 1, size=4, endian="little")

          ##CHECK FOR VERSION 07
          if(temp.VERSION == 07){
            reserved2.len <- 15

          }else {
            ##MARKER_POSITION
            ###EXTR_START, EXTR_END
            temp <- readBin(con, what = "double", 8, size = 4, endian = "little")
            MARKPOS_X1[id_row] <- temp[1]
            MARKPOS_Y1[id_row] <- temp[2]
            MARKPOS_X2[id_row] <- temp[3]
            MARKPOS_Y2[id_row] <- temp[4]
            MARKPOS_X3[id_row] <- temp[5]
            MARKPOS_Y3[id_row] <- temp[6]
            EXTR_START[id_row] <- temp[7]
            EXTR_END[id_row]   <- temp[8]
            reserved2.len <- 42
          }
        }# end RECTYPE 128
        temp.RESERVED2 <- readBin(con, what = "raw", reserved2.len, size = 1,
                                  endian = "little")
      }
    }

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## BIN FILE FORMAT SUPPORT
    else if (temp.VERSION == 03 || temp.VERSION == 04) {

      ##LTYPE
      LTYPE[id_row]<-readBin(con, what="int", 1, size=1, endian="little")

      ##LOW, HIGH, RATE
      temp <- readBin(con, what="double", 3, size=4, endian="little")
      LOW[id_row] <- temp[1]
      HIGH[id_row] <- temp[2]
      RATE[id_row] <- temp[3]

      ##XCOORD, YCOORD, TOLDELAY, TOLON, TOLOFF
      temp <- readBin(con, what = "integer", 6, size = 2, endian = "little")
      TEMPERATURE[id_row] <- temp[1]
      XCOORD[id_row]      <- temp[2]
      YCOORD[id_row]      <- temp[3]
      TOLDELAY[id_row]    <- temp[4]
      TOLON[id_row]       <- temp[5]
      TOLOFF[id_row]      <- temp[6]

      ##POSITION
      ##RUN
      temp <- readBin(con, what = "integer", 2, size = 1, endian = "little",
                      signed = FALSE)
      POSITION[id_row] <- temp[1]
      RUN[id_row] <- temp[2]

      ## TIME, DATE, SEQUENCE, USER
      TIME[id_row] <- .read_string(con, 7, force.size = 6)
      DATE[id_row] <- .read_string(con, 7, force.size = 6)
      SEQUENCE[id_row] <- .read_string(con, 9)
      USER[id_row] <- .read_string(con, 9)

      ##DTYPE
      DTYPE[id_row] <- readBin(con, what="int", 1, size=1, endian="little")

      ##IRR_TIME
      IRR_TIME[id_row] <- readBin(con, what="double", 1, size=4, endian="little")

      ##IRR_TYPE
      ##IRR_UNIT
      temp <- readBin(con, what = "integer", 2, size = 1, endian = "little")
      IRR_TYPE[id_row] <- temp[1]
      IRR_UNIT[id_row] <- temp[2]

      ##BL_TIME
      BL_TIME[id_row] <- readBin(con, what="double", 1, size=4, endian="little")

      ##BL_UNIT
      BL_UNIT[id_row] <- readBin(con, what="int", 1, size=1, endian="little")

      ##AN_TEMP, AN_TIME, NORM1, NORM2, NORM3, BG
      temp <- readBin(con, what="double", 6, size=4, endian="little")
      AN_TEMP[id_row] <- temp[1]
      AN_TIME[id_row] <- temp[2]
      NORM1[id_row] <- temp[3]
      NORM2[id_row] <- temp[4]
      NORM3[id_row] <- temp[5]
      BG[id_row] <- temp[6]

      ##SHIFT
      SHIFT[id_row] <- readBin(con, what="integer", 1, size=2, endian="little")

      ## SAMPLE, COMMENT
      SAMPLE[id_row]  <- .read_string(con, 21)
      COMMENT[id_row] <- .read_string(con, 81)

      ##LIGHTSOURCE, SET, TAG
      temp <- readBin(con, what="int", 3, size=1, endian="little")
      LIGHTSOURCE[id_row] <- temp[1]
      SET[id_row] <- temp[2]
      TAG[id_row] <- temp[3]

      ##GRAIN
      GRAINNUMBER[id_row] <- readBin(con, what="int", 1, size=2, endian="little")

      ##LPOWER
      LIGHTPOWER[id_row] <- readBin(con, what="double", 1, size=4, endian="little")

      ##SYSTEMID
      SYSTEMID[id_row] <- readBin(con, what="integer", 1, size=2, endian="little")

      ##Unfortunately an inconsitent BIN-file structure forces a differenciation ...
      if(temp.VERSION == 03){
        ##RESERVED
        temp.RESERVED1 <- readBin(con, what="raw", 36, size=1, endian="little")

        ##ONTIME, OFFTIME
        temp <- readBin(con, what="double", 2, size=4, endian="little")
        ONTIME[id_row] <- temp[1]
        OFFTIME[id_row] <- temp[2]

        ##Enable flags  #GateEnabled for v 06
        GATE_ENABLED[id_row] <- as.numeric(readBin(con, what="raw", 1, size=1, endian="little"))

        ##ONGATEDELAY, OFFGATEDELAY
        temp <- readBin(con, what="double", 2, size=4, endian="little")
        GATE_START[id_row] <- temp[1]
        GATE_STOP[id_row] <- temp[2]

        ##RESERVED
        temp.RESERVED2<-readBin(con, what="raw", 1, size=1, endian="little")

      }else{
        ##RESERVED
        temp.RESERVED1<-readBin(con, what="raw", 20, size=1, endian="little")

        ##CURVENO
        CURVENO[id_row] <- readBin(con, what="integer", 1, size=2, endian="little")

        ##TIMETICK
        TIMETICK[id_row] <- readBin(con, what="double", 1, size=4, endian="little")

        ##ONTIME, STIMPERIOD
        temp <- readBin(con, what="integer", 2, size=4, endian="little")
        ONTIME[id_row] <- temp[1]
        STIMPERIOD[id_row] <- temp[2]

        ##GATE_ENABLED
        GATE_ENABLED[id_row] <- as.numeric(readBin(con, what="raw", 1, size=1, endian="little"))

        ##ONGATEDELAY, OFFGATEDELAY
        temp <- readBin(con, what="double", 2, size=4, endian="little")
        GATE_START[id_row] <- temp[1]
        GATE_STOP[id_row] <- temp[2]

        ##PTENABLED
        ##RESERVED
        temp <- readBin(con, what = "raw", 11, size = 1, endian = "little")
        PTENABLED[id_row] <- as.numeric(temp[1])
        temp.RESERVED2 <- temp[2:11]
      }
    }

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Unrecognised version
    ##
    ## We should already have raised a warning that the file is corrupt
    ## during the first scan of the BIN/BINX file: at that point we have
    ## set `n.records` so that we would stop reading before encountering
    ## again the record with unrecognised version number, so here we just
    ## assert that that is the case
    stopifnot(temp.VERSION %in% VERSIONS.supported)

     #DPOINTS
     if(temp.RECTYPE != 128) {
       temp.DPOINTS <- readBin(con, what = "integer", temp.NPOINTS, size = 4, endian = "little")

     } else {
       temp.DPOINTS <- lapply(1:temp.NPOINTS, function(x) {
         list(
           NOFPOINTS = readBin(con, what = "int", 1, size = 4, endian = "little"),
           USEDFOR = as.logical(readBin(con, what = "raw", 48, size = 1, endian = "little")),
           SHOWFOR = as.logical(readBin(con, what = "raw", 48, size = 1, endian = "little")),
           ROICOLOR = readBin(con, what = "integer", 1, size = 4, endian = "little"),
           X = readBin(con, what = "double", 50, size = 4, endian = "little"),
           Y = readBin(con, what = "double", 50, size = 4, endian = "little"))
       })
     }

    ## ----------------------------------------------------------------------
    ## close the current record

    temp.ID <- temp.ID + 1
    ID[id_row] <- temp.ID

    ## we set VERSION only now to its real value: as we've got here, we have
    ## read the entire record and decided to keep it
    VERSION[id_row]  <- as.numeric(temp.VERSION)

    ## update the progress bar
    if (txtProgressBar) {
      setTxtProgressBar(pb, seek.connection(con, origin = "current"))
    }

    results.DATA[[id_row]] <- temp.DPOINTS
    results.RESERVED[[id_row]][[1]] <- temp.RESERVED1
    results.RESERVED[[id_row]][[2]] <- temp.RESERVED2

    ## update id row
    id_row <- id_row + 1

  }#endwhile::end loop

  ##close
  if (txtProgressBar) {
    close(pb)
  }

  ## generate the final data.table
  results.METADATA <- data.table(
      ID,
      SEL = as.logical(TAG),
      VERSION,
      LENGTH,
      PREVIOUS,
      NPOINTS,
      RECTYPE,
      RUN,
      SET,
      POSITION,
      GRAIN = GRAINNUMBER,
      GRAINNUMBER,
      CURVENO,
      XCOORD,
      YCOORD,
      SAMPLE,
      COMMENT,
      SYSTEMID,
      FNAME,
      USER,
      TIME,
      DATE,
      DTYPE,
      BL_TIME,
      BL_UNIT,
      NORM1,
      NORM2,
      NORM3,
      BG,
      SHIFT,
      TAG,
      LTYPE,
      LIGHTSOURCE,
      LPOWER = LIGHTPOWER,
      LIGHTPOWER,
      LOW,
      HIGH,
      RATE,
      TEMPERATURE,
      MEASTEMP,
      AN_TEMP,
      AN_TIME,
      TOLDELAY,
      TOLON,
      TOLOFF,
      IRR_TIME,
      IRR_TYPE,
      IRR_UNIT,
      IRR_DOSERATE,
      IRR_DOSERATEERR,
      TIMESINCEIRR,
      TIMETICK,
      ONTIME,
      OFFTIME,
      STIMPERIOD,
      GATE_ENABLED,
      ENABLE_FLAGS = GATE_ENABLED,
      GATE_START,
      GATE_STOP,
      PTENABLED,
      DTENABLED,
      DEADTIME,
      MAXLPOWER,
      XRF_ACQTIME,
      XRF_HV,
      XRF_CURR,
      XRF_DEADTIMEF,
      DETECTOR_ID,
      LOWERFILTER_ID,
      UPPERFILTER_ID,
      ENOISEFACTOR,
      MARKPOS_X1, MARKPOS_Y1,
      MARKPOS_X2, MARKPOS_Y2,
      MARKPOS_X3, MARKPOS_Y3,
      EXTR_START,
      EXTR_END,
      SEQUENCE
    )

  ## remove NA values created by skipping records
  results.METADATA <- stats::na.omit(results.METADATA, cols = "VERSION")

  ##output
  if(verbose)
    message("\t >> ", length(results.DATA), " records read successfully\n")

  ## Record removals --------------------------------------------------------

  ## check if only the specified positions should be returned
  if(!is.null(position)){

    ##check whether the position is valid at all
    if (results.METADATA[, all(position %in% POSITION)]) {
      keep.positions <- results.METADATA[, POSITION %in% position]
      results.METADATA <- results.METADATA[keep.positions == TRUE, ]
      results.DATA <- results.DATA[keep.positions]
      results.RESERVED <- results.RESERVED[keep.positions]

      if (verbose) {
        message("[read_BIN2R()] Kept records matching 'position': ",
                .collapse(position, quote = FALSE))
      }
    }else{
      .throw_warning("At least one position number is not valid, ",
                     "valid position numbers are: ",
                     .collapse(results.METADATA[, unique(POSITION)],
                               quote = FALSE))
    }
  }

  ##check for position that have no data at all (error during the measurement)
  if(zero_data.rm){
    zero_data.check <- which(lengths(results.DATA) == 0)

    ##remove records if there is something to remove
    if (length(zero_data.check) > 0) {
      results.METADATA <- results.METADATA[-zero_data.check, ]
      results.DATA[zero_data.check] <- NULL
      results.RESERVED[zero_data.check] <- NULL

      .throw_warning("Zero-data records detected and removed: ",
                     .collapse(zero_data.check, quote = FALSE))
    }
  }

  ## if nothing is left, return an empty object
  if (nrow(results.METADATA) == 0) {
    if (verbose)
      message("[read_BIN2R()] Empty object returned")
    return(set_Risoe.BINfileData())
  }

  ##check for duplicated entries and remove them if wanted, but only if we have more than 2 records
  ##this check is skipped for results with a RECTYPE 128, which stems from camera measurements
  if (n.length >= 2 && length(results.DATA) >= 2 && all(results.METADATA[["RECTYPE"]] != 128)) {
    duplication.check <- suppressWarnings(which(c(
      0, vapply(
        2:length(results.DATA),
        FUN = function(x) {
          length(results.DATA[[x - 1]]) == length(results.DATA[[x]]) &&
          all(results.DATA[[x - 1]] == results.DATA[[x]])
        },
        FUN.VALUE = 1
      )
    ) == 1))
    if (length(duplication.check) != 0) {
      if (duplicated.rm) {
        ##remove records
        results.METADATA <- results.METADATA[-duplication.check, ]
        results.DATA[duplication.check] <- NULL
        results.RESERVED[duplication.check] <- NULL

        ##message
        if(verbose) {
          message("[read_BIN2R()] Duplicated records detected and removed: ",
                  .collapse(duplication.check, quote = FALSE))
        }

      } else{
        .throw_warning("Duplicated records detected: ",
                       .collapse(duplication.check, quote = FALSE),
                       "\n >> You should consider using 'duplicated.rm = TRUE'.")
      }
    }
  }

  ## recalculate ID as some records may not have been read if n.records
  ## was set or they were dropped in one of the previous blocks
  if (results.METADATA[, .N != n.length || max(ID) > n.length]) {
    results.METADATA[, ID := 1:.N]
    if (verbose)
      message("[read_BIN2R()] The record index has been recalculated")
  }

  # Convert Translation Matrix Values ---------------------------------------

  if (!show.raw.values) {
    ##LIGHTSOURCE CONVERSION
    results.METADATA[, LIGHTSOURCE := unname(LIGHTSOURCE.lookup[LIGHTSOURCE])]

    ##LTYPE CONVERSION
    results.METADATA[, LTYPE := unname(LTYPE.lookup[LTYPE])]

    ##DTYPE CONVERSION
    results.METADATA[, DTYPE := unname(DTYPE.lookup[DTYPE])]

    ## check for oddly set LTYPES, this may happen in old BIN-file versions
    if (results.METADATA$VERSION[1] == 3) {
      results.METADATA[LTYPE == "OSL" & LIGHTSOURCE == "IR diodes/IR Laser",
                       LTYPE := "IRSL"]
    }

    ##TIME CONVERSION, do not do for odd time formats as this could cause problems during export
    results.METADATA[nchar(TIME) == 5,
                     TIME := paste0("0", TIME)]
    results.METADATA[nchar(TIME) == 6,
                     TIME := format(as.POSIXct(TIME, format = "%H%M%S"),
                                    "%H:%M:%S")]
  }

  ## check for empty BIN-files names ... if so, set the name of the file as BIN-file name
  ## This can happen if the user uses different equipment
  if (results.METADATA[, all(is.na(FNAME))]) {
    results.METADATA[, FNAME := tools::file_path_sans_ext(basename(file))]
  }

  ## produce S4 object for output
  object <- set_Risoe.BINfileData(
    METADATA = results.METADATA,
    DATA = results.DATA,
    .RESERVED = results.RESERVED)

  ## Fast Forward -----------------------------------------------------------

  ## set fastForward to TRUE if arguments to Risoe.BINfileData2RLum.Analysis
  ## were specified
  if (!fastForward) {
    dots <- names(list(...))
    args <- dots[dots %in% names(formals(Risoe.BINfileData2RLum.Analysis))[-1]]
    if (length(args) > 0) {
      fastForward <- TRUE
      .throw_warning("Additional arguments specified: ", .collapse(args),
                     ", setting 'fastForward = TRUE'")
    }
  }

  ##return values
  ##with fast fastForward they will be converted directly to a list of RLum.Analysis objects
  if(fastForward){
     object <- Risoe.BINfileData2RLum.Analysis(object, ...)

     ##because we expect a list
     if(!inherits(object, "list"))
       object <- list(object)
  }

   return(object)
}

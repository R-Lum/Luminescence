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
#' Alternatively the input character can be just a directory (path), in this case the
#' the function tries to detect and import all BIN/BINX files found in the directory.
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
#' enables or disables [txtProgressBar]. Ignored if `verbose = FALSE`.
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
#' enables or disables verbose mode
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
  if (is.character(file)) {
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

    temp.return <- lapply(1:length(file), function(x) {
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

  # Config --------------------------------------------------------------------------------------
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
    message("[read_BIN2R()] File '", file, "' is a zero-byte file, ",
            "NULL returned")
    return(NULL)
  }

  ## check if file is a BIN or BINX file
  if(!any(tolower(tools::file_ext(file)) %in%  c("bin", "binx"))) {
    message("[read_BIN2R()] File '", file, "' is not a file of type ",
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
    if(temp.VERSION == 06 | temp.VERSION == 07 | temp.VERSION == 08){
      length.size <- 4
    }else{
      length.size <- 2
    }

    temp.LENGTH  <- readBin(con, what = "integer", 1, size = length.size,
                            endian = "little")
    num.toread <- max(0, temp.LENGTH - length.size - 2)
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

  ##PRESET VALUES
  temp.CURVENO <- NA
  temp.FNAME <- NA
  temp.MEASTEMP <- NA
  temp.IRR_UNIT <- NA
  temp.IRR_DOSERATE <- NA
  temp.IRR_DOSERATEERR <- NA
  temp.TIMESINCEIRR <- NA
  temp.TIMETICK <- NA
  temp.ONTIME <- NA
  temp.OFFTIME <- NA
  temp.STIMPERIOD <- NA
  temp.GATE_ENABLED <- raw(length = 1)
  temp.ENABLE_FLAGS <- raw(length = 1)
  temp.GATE_START <- NA
  temp.GATE_STOP <- NA
  temp.GATE_END <- NA
  temp.PTENABLED <- raw(length = 1)
  temp.DTENABLED <- raw(length = 1)
  temp.DEADTIME <- NA
  temp.MAXLPOWER <- NA
  temp.XRF_ACQTIME <- NA
  temp.XRF_HV <- NA
  temp.XRF_CURR <- NA
  temp.XRF_DEADTIMEF <- NA
  temp.DETECTOR_ID <- NA
  temp.LOWERFILTER_ID <- NA
  temp.UPPERFILTER_ID <- NA
  temp.ENOISEFACTOR <- NA
  temp.SEQUENCE <- NA
  temp.GRAIN <- NA
  temp.GRAINNUMBER <- NA
  temp.LIGHTPOWER <- NA
  temp.LPOWER <- NA
  temp.RECTYPE <- 0
  temp.MARKPOS_X1 <- NA
  temp.MARKPOS_Y1 <- NA
  temp.MARKPOS_X2 <- NA
  temp.MARKPOS_Y2 <- NA
  temp.MARKPOS_X3 <- NA
  temp.MARKPOS_Y3 <- NA
  temp.EXTR_START <- NA
  temp.EXTR_END <- NA

  ## set TIME_SIZE
  TIME_SIZE <- 0

  ##overwrite length if required
  if(!is.null(n.records))
    n.length <- length(n.records)

  ## set index for entry row in table
  id_row <- 1

  ## initialise default empty list
  results.METADATA.defaults <- list(
    ##1 to 7
    ID = 0,
    SEL = FALSE,
    VERSION = 0,
    LENGTH = 0L,
    PREVIOUS = 0L,
    NPOINTS = 0L,
    RECTYPE = 0L,

    #8 to 17
    RUN = 0L,
    SET = 0L,
    POSITION = 0L,
    GRAIN = 0L,
    GRAINNUMBER = 0L,
    CURVENO = 0L,
    XCOORD = 0L,
    YCOORD = 0L,
    SAMPLE = "",
    COMMENT = "",

    #18 to 22
    SYSTEMID = 0L,
    FNAME = "",
    USER = "",
    TIME = "",
    DATE = "",

    ##23 to 31
    DTYPE = NA_character_,
    BL_TIME = 0,
    BL_UNIT = 0L,
    NORM1 = 0,
    NORM2 = 0,
    NORM3 = 0,
    BG = 0,
    SHIFT = 0L,
    TAG = 0L,

    ##32 to 67
    LTYPE = NA_character_,
    LIGHTSOURCE = "",
    LPOWER = 0,
    LIGHTPOWER = 0,
    LOW = 0,
    HIGH = 0,
    RATE = 0,
    TEMPERATURE = 0,
    MEASTEMP = 0,
    AN_TEMP = 0,
    AN_TIME = 0,
    TOLDELAY = 0L,
    TOLON = 0L,
    TOLOFF = 0L,
    IRR_TIME = 0,
    IRR_TYPE = 0L,
    IRR_UNIT = 0L,
    IRR_DOSERATE = 0,
    IRR_DOSERATEERR = 0,
    TIMESINCEIRR = 0,
    TIMETICK = 0,
    ONTIME = 0,
    OFFTIME = 0,
    STIMPERIOD = 0L,
    GATE_ENABLED = 0,
    ENABLE_FLAGS = 0,
    GATE_START  = 0,
    GATE_STOP = 0,
    PTENABLED = 0,
    DTENABLED = 0,
    DEADTIME = 0,
    MAXLPOWER = 0,
    XRF_ACQTIME = 0,
    XRF_HV = 0,
    XRF_CURR = 0,
    XRF_DEADTIMEF = 0,

    #68 to 79
    DETECTOR_ID = 0L,
    LOWERFILTER_ID = 0L,
    UPPERFILTER_ID = 0L,
    ENOISEFACTOR = 0,
    MARKPOS_X1 = 0,
    MARKPOS_Y1 = 0,
    MARKPOS_X2 = 0,
    MARKPOS_Y2 = 0,
    MARKPOS_X3 = 0,
    MARKPOS_Y3 = 0,
    EXTR_START = 0,
    EXTR_END = 0,

    ##80
    SEQUENCE = ""
  )

  results.METADATA.list <- list(results.METADATA.defaults)

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

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # BINX FORMAT SUPPORT -----------------------------------------------------
    if(temp.VERSION == 05 | temp.VERSION == 06 | temp.VERSION == 07 | temp.VERSION == 08){
      ##(1) Header size and structure
      ##LENGTH, PREVIOUS, NPOINTS, LTYPE
      temp <- readBin(con, what = "int", 3, size = 4, endian = "little")
      temp.LENGTH <- temp[1]
      temp.PREVIOUS <- temp[2]
      temp.NPOINTS <- temp[3]

      ## skip record if not selected
      ## the first condition boosts the speed of reading if n.records is not
      ## used; otherwise for each record the condition is checked whether
      ## used or not.
      if(!is.null(n.records) && !(temp.ID + 1) %in% n.records) {
        temp.ID <- temp.ID + 1
        seek.connection(con, temp.LENGTH - 14, origin = "current")
        next()
      }

      #for temp.VERSION == 08
      #RECTYPE
      if(temp.VERSION == 08){
        temp.RECTYPE <- readBin(con, what = "int", 1, size = 1, endian = "little", signed = FALSE)

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
            if(!ignore.RECTYPE){
              .throw_error("Byte RECTYPE = ", temp.RECTYPE,
                           " is not supported in record #", temp.ID + 1, ", ",
                           "check your BIN/BINX file")
            } else {
              if(verbose)
                message("\n[read_BIN2R()] Byte RECTYPE = ", temp.RECTYPE,
                        " is not supported in record #", temp.ID + 1,
                        ", record skipped")

              ## update and jump to next record, to avoid further trouble
              ## we set the VERSION to NA and remove it later, otherwise we
              ## break expected functionality
              temp.ID <- temp.ID + 1
              results.METADATA.list[[length(results.METADATA.list) + 1]] <-
                modifyList(x = results.METADATA.defaults,
                           val = list(ID = temp.ID, VERSION = NA))
              next()
            }
        }
      }

      ## RECTYPE == 128
      ## If the RECTYPE is 128, only the the header bytes until here make any sense,
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
        temp.RUN <- temp[1]
        temp.SET <- temp[2]
        temp.POSITION <- temp[3]
        temp.GRAINNUMBER <- temp[4]
        temp.CURVENO <- temp[5]
        temp.XCOORD <- temp[6]
        temp.YCOORD <- temp[7]

        ##SAMPLE, COMMENT
        ##SAMPLE
        SAMPLE_SIZE <- readBin(con, what="int", 1, size=1, endian="little")
        temp.SAMPLE <- readChar(con, SAMPLE_SIZE, useBytes = TRUE)

        #however it should be set to 20
        #step forward in con
        if (SAMPLE_SIZE < 20) {
          seek.connection(con, 20 - SAMPLE_SIZE, origin = "current")
        }

        ##COMMENT
        COMMENT_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
        temp.COMMENT <- suppressWarnings(
          readChar(con, COMMENT_SIZE, useBytes=TRUE)) #set to 80 (manual)

        #step forward in con
        if (COMMENT_SIZE < 80) {
          seek.connection(con, 80 - COMMENT_SIZE, origin = "current")
        }

        ##(3) Instrument and sequence characteristic
        ##SYSTEMID
        temp.SYSTEMID <- readBin(con, what="int", 1, size=2, endian="little")

        ##FNAME
        FNAME_SIZE <- max(readBin(con, what = "integer", 1, size = 1,
                                  endian = "little"), 0)

        ##correct for 0 file name length
        if (FNAME_SIZE > 0) {
          temp.FNAME<-readChar(con, FNAME_SIZE, useBytes=TRUE) #set to 100 (manual)
        }

        #step forward in con
        if (FNAME_SIZE < 100) {
          seek.connection(con, 100 - FNAME_SIZE, origin = "current")
        }

        ##USER
        USER_SIZE <- max(readBin(con, what = "integer", 1, size = 1,
                                 endian = "little"), 0)

        ##correct for 0 user size length
        if (USER_SIZE > 0) {
          temp.USER <-
            suppressWarnings(readChar(con, USER_SIZE, useBytes = TRUE)) #set to 30 (manual)
        }

        #step forward in con
        if (USER_SIZE < 30) {
          seek.connection(con, 30 - USER_SIZE, origin = "current")
        }

        ##TIME
        TIME_SIZE <- max(readBin(con, what = "integer", 1, size = 1,
                                 endian = "little"), 0)

        ##time size corrections for wrong time formats; set n to 6 for all values
        ##according to the handbook by Geoff Duller, 2007
        if (TIME_SIZE > 0) {
          temp.TIME<-readChar(con, TIME_SIZE, useBytes=TRUE)

          ##correct the mess by others
          if(nchar(temp.TIME) == 5)
            temp.TIME <- paste0("0", temp.TIME)
        }

        if (TIME_SIZE < 6) {
          seek.connection(con, 6 - TIME_SIZE, origin = "current")
        }

        ##DATE
        DATE_SIZE<-readBin(con, what="int", 1, size=1, endian="little")

        ##date size corrections for wrong date formats; set n to 6 for all values
        ##according to the handbook of Geoff Duller, 2007
        DATE_SIZE<-6
        temp.DATE <- suppressWarnings(readChar(con, DATE_SIZE, useBytes = TRUE))

        ##(4) Analysis
        ##DTYPE
        temp.DTYPE<-readBin(con, what="int", 1, size=1, endian="little")

        ##BL_TIME
        temp.BL_TIME<-readBin(con, what="double", 1, size=4, endian="little")

        ##BL_UNIT
        temp.BL_UNIT<-readBin(con, what="int", 1, size=1, endian="little")

        ##NORM1, NORM2, NORM3, BG
        temp <- readBin(con, what="double", 4, size=4, endian="little")
        temp.NORM1 <- temp[1]
        temp.NORM2 <- temp[2]
        temp.NORM3 <- temp[3]
        temp.BG <- temp[4]

        ##SHIFT
        temp.SHIFT<- readBin(con, what="integer", 1, size=2, endian="little")

        ##TAG
        temp.TAG <- readBin(con, what="int", 1, size=1, endian="little")

        ##RESERVED
        temp.RESERVED1 <-readBin(con, what="raw", 20, size=1, endian="little")

        ##(5) Measurement characteristics

        ##LTYPE
        ##LTYPESOURCE
        temp <- readBin(con, what = "integer", 2, size = 1, endian = "little")
        temp.LTYPE <- temp[1]
        temp.LIGHTSOURCE <- temp[2]

        ##LIGHTPOWER, LOW, HIGH, RATE
        temp <- readBin(con, what="double", 4, size=4, endian="little")
        temp.LIGHTPOWER <- temp[1]
        temp.LOW <- temp[2]
        temp.HIGH <- temp[3]
        temp.RATE <- temp[4]

        ##TEMPERATURE
        ##MEASTEMP
        temp <- readBin(con, what = "integer", 2, size = 2, endian = "little")
        temp.TEMPERATURE <- temp[1]
        temp.MEASTEMP <- temp[2]

        ##AN_TEMP
        ##AN_TIME
        temp <- readBin(con, what = "double", 2, size = 4, endian = "little")
        temp.AN_TEMP <- temp[1]
        temp.AN_TIME <- temp[2]

        ##DELAY, ON, OFF
        temp <- readBin(con, what="int", 3, size=2, endian="little")
        temp.TOLDELAY <- temp[1]
        temp.TOLON <- temp[2]
        temp.TOLOFF <- temp[3]

        ##IRR_TIME
        temp.IRR_TIME <- readBin(con, what="double", 1, size=4, endian="little")

        ##IRR_TYPE
        temp.IRR_TYPE <- readBin(con, what="int", 1, size=1, endian="little")

        ##IRR_DOSERATE
        temp.IRR_DOSERATE <- readBin(con, what="double", 1, size=4, endian="little")

        ##IRR_DOSERATEERR
        if(temp.VERSION != 05)
          temp.IRR_DOSERATEERR <- readBin(con, what="double", 1, size=4, endian="little")

        ##TIMESINCEIRR
        temp.TIMESINCEIRR <- readBin(con, what="integer", 1, size=4, endian="little")

        ##TIMETICK
        temp.TIMETICK <- readBin(con, what="double", 1, size=4, endian="little")

        ##ONTIME
        ##STIMPERIOD
        temp <- readBin(con, what = "integer", 2, size = 4, endian = "little")
        temp.ONTIME <- temp[1]
        temp.STIMPERIOD <- temp[2]

        ##GATE_ENABLED
        temp.GATE_ENABLED <- readBin(con, what="raw", 1, size=1, endian="little")

        ##GATE_START
        ##GATE_STOP
        temp <- readBin(con, what = "integer", 2, size = 4, endian = "little")
        temp.GATE_START <- temp[1]
        temp.GATE_STOP <- temp[2]

        ##PTENABLED
        ##DTENABLED
        temp <- readBin(con, what = "raw", 2, size = 1, endian = "little")
        temp.PTENABLED <- temp[1]
        temp.DTENABLED <- temp[2]

        ##DEADTIME, MAXLPOWER, XRF_ACQTIME, XRF_HV
        temp <- readBin(con, what="double", 4, size=4, endian="little")
        temp.DEADTIME <- temp[1]
        temp.MAXLPOWER <- temp[2]
        temp.XRF_ACQTIME <- temp[3]
        temp.XRF_HV <- temp[4]

        ##XRF_CURR
        temp.XRF_CURR <- readBin(con, what="integer", 1, size=4, endian="little")

        ##XRF_DEADTIMEF
        temp.XRF_DEADTIMEF <- readBin(con, what="double", 1, size=4, endian="little")

        ###Account for differences between V5, V6 and V7
        if(temp.VERSION == 06){
          ##RESERVED
          temp.RESERVED2<-readBin(con, what="raw", 24, size=1, endian="little")

        }else if(temp.VERSION == 05){
          ##RESERVED
          temp.RESERVED2<-readBin(con, what="raw", 4, size=1, endian="little")

        }else{

          ##DETECTOR_ID
          temp.DETECTOR_ID <- readBin(con, what="int", 1, size=1, endian="little")

          ##LOWERFILTER_ID, UPPERFILTER_ID
          temp <- readBin(con, what="int", 2, size=2, endian="little")
          temp.LOWERFILTER_ID <- temp[1]
          temp.UPPERFILTER_ID <- temp[2]

          ##ENOISEFACTOR
          temp.ENOISEFACTOR <- readBin(con, what="double", 1, size=4, endian="little")

          ##CHECK FOR VERSION 07
          if(temp.VERSION == 07){
            temp.RESERVED2<-readBin(con, what="raw", 15, size=1, endian="little")

          }else {
            ##MARKER_POSITION
            ###EXTR_START, EXTR_END
            temp <- readBin(con, what = "double", 8, size = 4, endian = "little")
              temp.MARPOS_X1 <- temp[1]
              temp.MARPOS_Y1 <- temp[2]
              temp.MARPOS_X2 <- temp[3]
              temp.MARPOS_Y2 <- temp[4]
              temp.MARPOS_X3 <- temp[5]
              temp.MARPOS_Y3 <- temp[6]
              temp.EXTR_START <- temp[7]
              temp.EXTR_END <- temp[8]

            temp.RESERVED2<-readBin(con, what="raw", 42, size=1, endian="little")
          }
        }# end RECTYPE 128
      }
    }else if(temp.VERSION == 04 | temp.VERSION == 03){
      ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ##START BIN FILE FORMAT SUPPORT  (vers. 03 and 04)
      ##LENGTH, PREVIOUS, NPOINTS, LTYPE
      temp <- readBin(con, what="int", 3, size=2, endian="little")
      temp.LENGTH <- temp[1]
      temp.PREVIOUS <- temp[2]
      temp.NPOINTS <- temp[3]

      ## set temp ID if within select
      if(!is.null(n.records) && !(temp.ID + 1) %in% n.records) {
        temp.ID <- temp.ID + 1
        seek.connection(con, temp.LENGTH - 8, origin = "current")
        next()
      }

      ##LTYPE
      temp.LTYPE<-readBin(con, what="int", 1, size=1, endian="little")

      ##LOW, HIGH, RATE
      temp <- readBin(con, what="double", 3, size=4, endian="little")
      temp.LOW <- temp[1]
      temp.HIGH <- temp[2]
      temp.RATE <- temp[3]

      ##XCOORD, YCOORD, TOLDELAY, TOLON, TOLOFF
      temp <- readBin(con, what = "integer", 6, size = 2, endian = "little")
      temp.TEMPERATURE <- temp[1]
      temp.XCOORD <- temp[2]
      temp.YCOORD <- temp[3]
      temp.TOLDELAY <- temp[4]
      temp.TOLON <- temp[5]
      temp.TOLOFF <- temp[6]

      ##POSITION
      ##RUN
      temp <- readBin(con, what = "integer", 2, size = 1, endian = "little",
                      signed = FALSE)
      temp.POSITION <- temp[1]
      temp.RUN <- temp[2]

      ##TIME
      TIME_SIZE <- readBin(
        con, what="int", 1, size=1, endian="little")

      ##time size corrections for wrong time formats; set n to 6 for all values
      ##according to the handbook of Geoff Duller, 2007
      TIME_SIZE<-6
      temp.TIME<-readChar(con, TIME_SIZE, useBytes=TRUE)

      ##DATE
      DATE_SIZE<-readBin(con, what="int", 1, size=1, endian="little")

      ##date size corrections for wrong date formats; set n to 6 for all values
      ##according to the handbook of Geoff Duller, 2007
      DATE_SIZE<-6
      temp.DATE<-readChar(con, DATE_SIZE, useBytes=TRUE)


      ##SEQUENCE
      SEQUENCE_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
      temp.SEQUENCE<-readChar(con, SEQUENCE_SIZE, useBytes=TRUE)

      #step forward in con
      if (SEQUENCE_SIZE < 8) {
        seek.connection(con, 8 - SEQUENCE_SIZE, origin = "current")
      }

      ##USER
      USER_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
      temp.USER<-readChar(con, USER_SIZE, useBytes=FALSE)

      #step forward in con
      if (USER_SIZE < 8) {
        seek.connection(con, 8 - USER_SIZE, origin = "current")
      }

      ##DTYPE
      temp.DTYPE <- readBin(con, what="int", 1, size=1, endian="little")

      ##IRR_TIME
      temp.IRR_TIME <- readBin(con, what="double", 1, size=4, endian="little")

      ##IRR_TYPE
      ##IRR_UNIT
      temp <- readBin(con, what = "integer", 2, size = 1, endian = "little")
      temp.IRR_TYPE <- temp[1]
      temp.IRR_UNIT <- temp[2]

      ##BL_TIME
      temp.BL_TIME<-readBin(con, what="double", 1, size=4, endian="little")

      ##BL_UNIT
      temp.BL_UNIT<-readBin(con, what="int", 1, size=1, endian="little")

      ##AN_TEMP, AN_TIME, NORM1, NORM2, NORM3, BG
      temp <- readBin(con, what="double", 6, size=4, endian="little")
      temp.AN_TEMP <- temp[1]
      temp.AN_TIME <- temp[2]
      temp.NORM1 <- temp[3]
      temp.NORM2 <- temp[4]
      temp.NORM3 <- temp[5]
      temp.BG <- temp[6]

      ##SHIFT
      temp.SHIFT<-readBin(con, what="integer", 1, size=2, endian="little")

      ##SAMPLE
      SAMPLE_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
      temp.SAMPLE<-readChar(con, SAMPLE_SIZE, useBytes=TRUE) #however it should be set to 20

      #step forward in con
      if (SAMPLE_SIZE < 20) {
        seek.connection(con, 20 - SAMPLE_SIZE, origin = "current")
      }

      ##COMMENT
      COMMENT_SIZE <- readBin(con, what="int", 1, size=1, endian="little")
      temp.COMMENT <- readChar(con, COMMENT_SIZE, useBytes=TRUE) #set to 80 (manual)

      #step forward in con
      if (COMMENT_SIZE < 80) {
        seek.connection(con, 80 - COMMENT_SIZE, origin = "current")
      }

      ##LIGHTSOURCE, SET, TAG
      temp <- readBin(con, what="int", 3, size=1, endian="little")
      temp.LIGHTSOURCE <- temp[1]
      temp.SET <- temp[2]
      temp.TAG <- temp[3]

      ##GRAIN
      temp.GRAIN<-readBin(con, what="integer", 1, size=2, endian="little")

      ##LPOWER
      temp.LPOWER<-readBin(con, what="double", 1, size=4, endian="little")

      ##SYSTEMID
      temp.SYSTEMID<-readBin(con, what="integer", 1, size=2, endian="little")

      ##Unfortunately an inconsitent BIN-file structure forces a differenciation ...
      if(temp.VERSION == 03){
        ##RESERVED
        temp.RESERVED1<-readBin(con, what="raw", 36, size=1, endian="little")

        ##ONTIME, OFFTIME
        temp <- readBin(con, what="double", 2, size=4, endian="little")
        temp.ONTIME <- temp[1]
        temp.OFFTIME <- temp[2]

        ##Enable flags  #GateEnabled for v 06
        temp.ENABLE_FLAGS <- readBin(con, what="raw", 1, size=1, endian="little")
        temp.GATE_ENABLED <- temp.ENABLE_FLAGS

        ##ONGATEDELAY, OFFGATEDELAY
        temp <- readBin(con, what="double", 2, size=4, endian="little")
        temp.GATE_START <- temp[1]
        temp.GATE_STOP <- temp[2]

        ##RESERVED
        temp.RESERVED2<-readBin(con, what="raw", 1, size=1, endian="little")

      }else{
        ##RESERVED
        temp.RESERVED1<-readBin(con, what="raw", 20, size=1, endian="little")

        ##CURVENO
        temp.CURVENO <- readBin(con, what="integer", 1, size=2, endian="little")

        ##TIMETICK
        temp.TIMETICK <- readBin(con, what="double", 1, size=4, endian="little")

        ##ONTIME, STIMPERIOD
        temp <- readBin(con, what="integer", 2, size=4, endian="little")
        temp.ONTIME <- temp[1]
        temp.STIMPERIOD <- temp[2]

        ##GATE_ENABLED
        temp.GATE_ENABLED <- readBin(con, what="raw", 1, size=1, endian="little")

        ##ONGATEDELAY, OFFGATEDELAY
        temp <- readBin(con, what="double", 2, size=4, endian="little")
        temp.GATE_START <- temp[1]
        temp.GATE_END <- temp[2]
        temp.GATE_STOP <- temp.GATE_END

        ##PTENABLED
        ##RESERVED
        temp <- readBin(con, what = "raw", 11, size = 1, endian = "little")
        temp.PTENABLED <- temp[1]
        temp.RESERVED2 <- temp[2:11]
      }
    }

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

    #endif:format support
    ##END BIN FILE FORMAT SUPPORT
    ## ==========================================================================#
    #SET UNIQUE ID
    temp.ID <- temp.ID + 1

     ##update progress bar
    if (txtProgressBar) {
      setTxtProgressBar(pb, seek.connection(con, origin = "current"))
    }

    ##set for equal values with different names
    if(!is.na(temp.GRAINNUMBER)){temp.GRAIN <- temp.GRAINNUMBER}
    if(!is.na(temp.GRAIN)){temp.GRAINNUMBER <- temp.GRAIN}

    if(!is.na(temp.LIGHTPOWER)){temp.LPOWER <- temp.LIGHTPOWER}
    if(!is.na(temp.LPOWER)){temp.LIGHTPOWER <- temp.LPOWER}

    temp.SEL <- if(temp.TAG == 1) TRUE else FALSE

    ##replace values in the data.table with values
    results.METADATA.list[[length(results.METADATA.list) + 1]] <- list(
      ID = temp.ID,
      SEL = temp.SEL,
      VERSION = as.numeric(temp.VERSION),
      LENGTH = temp.LENGTH,
      PREVIOUS = temp.PREVIOUS,
      NPOINTS = temp.NPOINTS,
      RECTYPE = temp.RECTYPE,
      RUN = temp.RUN,
      SET = temp.SET,
      POSITION = temp.POSITION,
      GRAIN = temp.GRAIN,
      GRAINNUMBER = temp.GRAINNUMBER,
      CURVENO = temp.CURVENO,
      XCOORD = temp.XCOORD,
      YCOORD = temp.YCOORD,
      SAMPLE = temp.SAMPLE,
      COMMENT = temp.COMMENT,
      SYSTEMID = temp.SYSTEMID,
      FNAME = temp.FNAME,
      USER = temp.USER,
      TIME = temp.TIME,
      DATE = temp.DATE,
      DTYPE = as.character(temp.DTYPE),
      BL_TIME = temp.BL_TIME,
      BL_UNIT = temp.BL_UNIT,
      NORM1 = temp.NORM1,
      NORM2 = temp.NORM2,
      NORM3 = temp.NORM3,
      BG = temp.BG,
      SHIFT = temp.SHIFT,
      TAG = temp.TAG,
      LTYPE = as.character(temp.LTYPE),
      LIGHTSOURCE = as.character(temp.LIGHTSOURCE),
      LPOWER = temp.LPOWER,
      LIGHTPOWER = temp.LIGHTPOWER,
      LOW = temp.LOW,
      HIGH = temp.HIGH,
      RATE = temp.RATE,
      TEMPERATURE = temp.TEMPERATURE,
      MEASTEMP = temp.MEASTEMP,
      AN_TEMP = temp.AN_TEMP,
      AN_TIME = temp.AN_TIME,
      TOLDELAY = temp.TOLDELAY,
      TOLON = temp.TOLON,
      TOLOFF = temp.TOLOFF,
      IRR_TIME = temp.IRR_TIME,
      IRR_TYPE = temp.IRR_TYPE,
      IRR_UNIT = temp.IRR_UNIT,
      IRR_DOSERATE = temp.IRR_DOSERATE,
      IRR_DOSERATEERR = temp.IRR_DOSERATEERR,
      TIMESINCEIRR = temp.TIMESINCEIRR,
      TIMETICK = temp.TIMETICK,
      ONTIME = temp.ONTIME,
      OFFTIME = temp.OFFTIME,
      STIMPERIOD = temp.STIMPERIOD,
      GATE_ENABLED = as.numeric(temp.GATE_ENABLED),
      ENABLE_FLAGS = as.numeric(temp.ENABLE_FLAGS),
      GATE_START = temp.GATE_START,
      GATE_STOP = temp.GATE_STOP,
      PTENABLED = as.numeric(temp.PTENABLED),
      DTENABLED = as.numeric(temp.DTENABLED),
      DEADTIME = temp.DEADTIME,
      MAXLPOWER = temp.MAXLPOWER,
      XRF_ACQTIME = temp.XRF_ACQTIME,
      XRF_HV = temp.XRF_HV,
      XRF_CURR = temp.XRF_CURR,
      XRF_DEADTIMEF = temp.XRF_DEADTIMEF,
      DETECTOR_ID = temp.DETECTOR_ID,
      LOWERFILTER_ID = temp.LOWERFILTER_ID,
      UPPERFILTER_ID = temp.UPPERFILTER_ID,
      ENOISEFACTOR = temp.ENOISEFACTOR,
      MARKPOS_X1 = temp.MARKPOS_X1,
      MARKPOS_Y1 = temp.MARKPOS_Y1,
      MARKPOS_X2 = temp.MARKPOS_X2,
      MARKPOS_Y2 = temp.MARKPOS_Y2,
      MARKPOS_X3 = temp.MARKPOS_X3,
      MARKPOS_Y3 = temp.MARKPOS_Y3,

      ## FIXME(mcol): these two fields were not present when we were building
      ## up a data.table directly, so to reproduce exactly the objects as
      ## the previous code, we set them to 0, but arguably this is not
      ## correct
      EXTR_START = 0, # temp.EXTR_START,
      EXTR_END = 0, # temp.EXTR_END,

      SEQUENCE = temp.SEQUENCE
    )

    results.DATA[[id_row]] <- temp.DPOINTS

    results.RESERVED[[id_row]][[1]] <- temp.RESERVED1
    results.RESERVED[[id_row]][[2]] <- temp.RESERVED2

    ##reset values
    temp.GRAINNUMBER <- NA
    temp.GRAIN <- NA

    ## update id row
    id_row <- id_row + 1

  }#endwhile::end loop

  ##close
  if (txtProgressBar) {
    close(pb)
  }

  ## remove NA values created by skipping records
  results.METADATA <- rbindlist(results.METADATA.list)
  results.METADATA <- na.omit(results.METADATA, cols = "VERSION")

  ## remove the first row (default) unless it's the only one left
  if (nrow(results.METADATA) > 1) {
    results.METADATA <- results.METADATA[-1]
  }

  ##output
  if(verbose)
    message("\t >> ", length(results.DATA), " records read successfully\n")

  ## Record removals --------------------------------------------------------

  ## check if only the specified positions should be returned
  if(!is.null(position)){
    ##check whether the position is valid at all
    if (all(position %in% results.METADATA[["POSITION"]])) {
      keep.positions <- results.METADATA[["POSITION"]] %in% position
      results.METADATA <- results.METADATA[keep.positions, ]
      results.DATA <- results.DATA[keep.positions]
      results.RESERVED <- results.RESERVED[keep.positions]

      if (verbose) {
        message("[read_BIN2R()] Kept records matching 'position': ",
                .collapse(position, quote = FALSE))
      }
    }else{
      .throw_warning("At least one position number is not valid, ",
                     "valid position numbers are: ",
                     .collapse(unique(results.METADATA[["POSITION"]]),
                               quote = FALSE))
    }
  }

  ## recalculate ID as some records may not have been read if n.records was set
  ## or were dropped by position in the previous block
  results.METADATA[["ID"]] <- 1:nrow(results.METADATA)
  if (verbose)
    message("[read_BIN2R()] The record index has been recalculated")

  ##check for position that have no data at all (error during the measurement)
  if(zero_data.rm){
    zero_data.check <- which(vapply(results.DATA, length, numeric(1)) == 0)

    ##remove records if there is something to remove
    if(length(zero_data.check) != 0){
      results.METADATA <- results.METADATA[-zero_data.check, ]
      results.DATA[zero_data.check] <- NULL
      results.RESERVED[zero_data.check] <- NULL

      ## if nothing is left, return an empty object
      if(nrow(results.METADATA) == 0)
        return(set_Risoe.BINfileData())

      ##recalculate record index
      results.METADATA[["ID"]] <- 1:nrow(results.METADATA)

      .throw_warning("Zero-data records detected and removed: ",
                     .collapse(zero_data.check, quote = FALSE),
                     ", record index recalculated")
    }
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

        ##recalculate record index
        results.METADATA[["ID"]] <- 1:nrow(results.METADATA)

        ##message
        if(verbose) {
          message("[read_BIN2R()] Duplicated records detected and removed: ",
                  .collapse(duplication.check, quote = FALSE),
                  ", record index recalculated")
        }

      } else{
        .throw_warning("Duplicated records detected: ",
                       .collapse(duplication.check, quote = FALSE),
                       "\n >> You should consider using 'duplicated.rm = TRUE'.")
      }
    }
  }

  ##produce S4 object for output
  object <- set_Risoe.BINfileData(
    METADATA = results.METADATA,
    DATA = results.DATA,
    .RESERVED =  results.RESERVED)

  if (length(object) == 0) {
    if (verbose) {
      message("[read_BIN2R()] Empty object returned")
    }
    return(object)
  }

  # Convert Translation Matrix Values ---------------------------------------
  if (!show.raw.values) {
    ##LIGHTSOURCE CONVERSION
    object@METADATA[["LIGHTSOURCE"]] <-
      unname(LIGHTSOURCE.lookup[object@METADATA[["LIGHTSOURCE"]]])

    ##LTYPE CONVERSION
    object@METADATA[["LTYPE"]] <-
      unname(LTYPE.lookup[object@METADATA[["LTYPE"]]])

    ##DTYPE CONVERSION
    object@METADATA[["DTYPE"]] <-
      unname(DTYPE.lookup[object@METADATA[["DTYPE"]]])

        ##CHECK for oddly set LTYPES, this may happen in old BIN-file versions
        if (object@METADATA[["VERSION"]][1] == 3) {
          object@METADATA[["LTYPE"]] <-
            sapply(1:length(object@METADATA[["LTYPE"]]), function(x) {
              if (object@METADATA[["LTYPE"]][x] == "OSL" &
                  object@METADATA[["LIGHTSOURCE"]][x] == "IR diodes/IR Laser") {
                return("IRSL")

              } else{
                return(object@METADATA[["LTYPE"]][x])
              }
            })
        }

    ##TIME CONVERSION, do not do for odd time formats as this could cause problems during export
    if (TIME_SIZE == 6) {
      object@METADATA[["TIME"]] <-
        format(strptime(as.character(object@METADATA[["TIME"]]), "%H%M%S"), "%H:%M:%S")
    }
  }

  ## check for empty BIN-files names ... if so, set the name of the file as BIN-file name
  ## This can happen if the user uses different equipment
  if(all(is.na(object@METADATA[["FNAME"]]))){
    object@METADATA[["FNAME"]] <- strsplit(x = basename(file), split = ".", fixed = TRUE)[[1]][1]
  }

  # Fast Forward --------------------------------------------------------------------------------
  ## set fastForward to TRUE if one of this arguments is used
  if(any(names(list(...)) %in% names(formals(Risoe.BINfileData2RLum.Analysis))[-1]) &
     fastForward == FALSE) {
    fastForward <- TRUE
    .throw_warning("Automatically reset 'fastForward = TRUE'")
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

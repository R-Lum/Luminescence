#' @title Import Princeton Instruments (TM) SPE-file into R
#'
#' @description Function imports Princeton Instruments (TM) SPE-files into R environment and
#' provides [RLum.Data.Image-class] objects as output.
#'
#' @details Function provides an R only import routine for the Princeton Instruments
#' SPE format. Import functionality is based on the file format description provided by
#' Princeton Instruments and a MatLab script written by Carl Hall (see
#' references).
#'
#' @param file [character] (**required**):
#' SPE-file name (including path), e.g.
#' - `[WIN]`: `read_SPE2R("C:/Desktop/test.spe")`
#' - `[MAC/LINUX]`: `read_SPE2R("/User/test/Desktop/test.spe")`.
#' Additionally, it can be a URL starting with http:// or https://.
#'
#' @param output.object [character] (*with default*):
#' set `RLum` output object.  Allowed types are `"RLum.Data.Spectrum"`,
#' `"RLum.Data.Image"` or `"matrix"`
#'
#' @param frame.range [vector] (*optional*):
#' limit frame range, e.g. select first 100 frames by `frame.range = c(1,100)`
#'
#' @param txtProgressBar [logical] (*with default*):
#' enable/disable the progress bar. Ignored if `verbose = FALSE`.
#'
#' @param verbose [logical] (*with default*): enable/disable output to the
#' terminal.
#'
#' @param ... not used, for compatibility reasons only
#'
#' @return
#' Depending on the chosen option the functions returns three different
#' type of objects:
#'
#' `output.object`
#'
#' `RLum.Data.Spectrum`
#'
#' An object of type [RLum.Data.Spectrum-class] is returned.  Row
#' sums are used to integrate all counts over one channel.
#'
#' `RLum.Data.Image`
#'
#' An object of type [RLum.Data.Image-class] is returned.  Due to
#' performance reasons the import is aborted for files containing more than 100
#' frames. This limitation can be overwritten manually by using the argument
#' `frame.range`.
#'
#' `matrix`
#'
#' Returns a matrix of the form: Rows = Channels, columns = Frames. For the
#' transformation the function [get_RLum] is used,
#' meaning that the same results can be obtained by using the function
#' [get_RLum] on an `RLum.Data.Spectrum` or `RLum.Data.Image` object.
#'
#' @note
#' **The function does not test whether the input data are spectra or pictures for spatial resolved analysis!**
#'
#' The function has been successfully tested for SPE format versions 2.x.
#'
#' *Currently not all information provided by the SPE format are supported.*
#'
#' @section Function version: 0.1.5
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [readBin], [RLum.Data.Spectrum-class]
#'
#' @references
#' Princeton Instruments, 2014. Princeton Instruments SPE 3.0 File
#' Format Specification, Version 1.A (for document URL please use an internet search machine)
#'
#' Hall, C., 2012: readSPE.m.
#' `https://www.mathworks.com/matlabcentral/fileexchange/35940-readspe`
#'
#' @keywords IO
#'
#' @examples
#'
#' ## to run examples uncomment lines and run the code
#'
#' ##(1) Import data as RLum.Data.Spectrum object
#' #file <- file.choose()
#' #temp <- read_SPE2R(file)
#' #temp
#'
#' ##(2) Import data as RLum.Data.Image object
#' #file <- file.choose()
#' #temp <- read_SPE2R(file, output.object = "RLum.Data.Image")
#' #temp
#'
#' ##(3) Import data as matrix object
#' #file <- file.choose()
#' #temp <- read_SPE2R(file, output.object = "matrix")
#' #temp
#'
#' ##(4) Export raw data to csv, if temp is a RLum.Data.Spectrum object
#' # write.table(x = get_RLum(temp),
#' #             file = "[your path and filename]",
#' #             sep = ";", row.names = FALSE)
#'
#'
#' @md
#' @export
read_SPE2R <- function(
  file,
  output.object = "RLum.Data.Image",
  frame.range,
  txtProgressBar = TRUE,
  verbose = TRUE,
  ...
) {
  .set_function_name("read_SPE2R")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(file, "character")
  .validate_length(file, 1)
  valid.output.object <- c("RLum.Data.Image", "RLum.Data.Spectrum", "matrix")
  .validate_args(output.object, valid.output.object)

  ##check if file exists
  if(!file.exists(file)){
    failed <- TRUE

    ## check if the file is an URL ... you never know
    if (grepl(pattern = "^https?://", x = file)) {
      if(verbose){
        cat("[read_SPE2R()] URL detected, checking connection ... ")
      }

      ##check URL
      if(!httr::http_error(file)){
        if (verbose) cat("OK\n")

        ##download file
        file_link <- tempfile("read_SPE2R_FILE", fileext = ".SPE")
        download.file(file, destfile = file_link, quiet = !verbose, mode = "wb")
        file <- file_link
        failed <- FALSE
      }else{
        if (verbose) cat("FAILED\n")
      }
    }

    if (failed) {
      .throw_message("File does not exist, NULL returned")
      return(NULL)
    }
  }

  ##check file extension
  if(!grepl(basename(file), pattern = "SPE$", ignore.case = TRUE)){
    if(strsplit(file, split = "\\.")[[1]][2] != "SPE"){
      .throw_error("Unsupported file format: *.",
                   strsplit(file, split = "\\.")[[1]][2], sep = "")
  }}


  if (!verbose)
    txtProgressBar <- FALSE

  # Open Connection ---------------------------------------------------------

  con <- file(file, "rb")

  if (verbose) {
    cat("\n[read_SPE2R()] Importing ...")
    cat("\n path: ", dirname(file))
    cat("\n file: ", .shorten_filename(basename(file)))
    cat("\n")
  }

  # read header -------------------------------------------------------------

  temp <- readBin(con, what="int", 2, size=2, endian="little", signed = TRUE)
  ControllerVersion <- temp[1] #Hardware version
  LogicOutput <- temp[2] #Definition of Output BNC

  temp <- readBin(con, what="int", 2, size=2, endian="little", signed = FALSE)
  AmpHiCapLowNoise <- temp[1] #Amp Switching Mode
  xDimDet <- temp[2] #Detector x dimension of chip.

  #timing mode
  mode <- readBin(con, what="int", 1, size=2, endian="little", signed = TRUE)

  #alternative exposure, in sec.
  exp_sec <- readBin(con, what="double", 1, size=4, endian="little")

  temp <- readBin(con, what="int", 2, size=2, endian="little", signed = TRUE)
  VChipXdim <- temp[1] # Virtual Chip X dim
  VChipYdim <- temp[2] # Virtual Chip Y dim

  #y dimension of CCD or detector.
  yDimDet <- readBin(con, what="int", 1, size=2, endian="little", signed = TRUE)

  #Date
  Date <- suppressWarnings(readChar(con, 10, useBytes=TRUE))

  ##jump
  stepping <- readBin(con, what="raw", 4, size=1, endian="little", signed = TRUE)

  #Old number of scans - should always be -1
  noscan <- readBin(con, what="int", 1, size=2, endian="little", signed = TRUE)

  #Detector Temperature Set
  DetTemperature <- readBin(con, what="double", 1, size=4, endian="little")

  # CCD/DiodeArray type
  DetType <- readBin(con, what="int", 1, size=2, endian="little", signed = TRUE)

  #actual # of pixels on x axis
  xdim <- readBin(con, what="int", 1, size=2, endian="little", signed = FALSE)

  ##jump
  stepping <- readBin(con, what="raw", 64, size=1, endian="little", signed = TRUE)

  ##experiment data type
  ##0 = 32f (4 bytes)
  ##1 = 32s (4 bytes)
  ##3 = 16u (2 bytes)
  ##8 = 32u (4 bytes)
  datatype <- readBin(con, what="int", 1, size=2, endian="little", signed = TRUE)

  ##jump
  stepping <- readBin(con, what="raw", 546, size=1, endian="little")

  #y dimension of raw data.
  ydim <- readBin(con, what="int", 1, size=2, endian="little", signed = FALSE)

  ##0=scrambled,1=unscrambled
  scramble <- readBin(con, what="int", 1, size=2, endian="little", signed = FALSE)

  ##jump
  stepping <- readBin(con, what="raw", 4, size=1, endian="little")

  #Number of scans (Early WinX)
  lnoscan <- readBin(con, what="int", 1, size=4, endian="little", signed = TRUE)

  #Number of Accumulations
  lavgexp <- readBin(con, what="int", 1, size=4, endian="little", signed = TRUE)

  ##Experiment readout time
  ReadoutTime <- readBin(con, what="double", 1, size=4, endian="little")

  #T/F Triggered Timing Option
  TriggeredModeFlag <- readBin(con, what="int", 1, size=2, endian="little", signed = TRUE)

  ##jump
  stepping <- readBin(con, what="raw", 768, size=1, endian="little")

  ##number of frames in file.
  NumFrames <- readBin(con, what="int", 1, size=4, endian="little", signed = TRUE)

  if(NumFrames > 100 & missing(frame.range) & output.object == "RLum.Data.Image"){
    .throw_error("Import aborted: this file containes > 100 frames (",
                 NumFrames, "). Use argument 'frame.range' to force import.")
  }

  ##set frame.range
  if(missing(frame.range) == TRUE){frame.range <- c(1,NumFrames)}

  ##jump
  stepping <- readBin(con, what="raw", 542, size=1, endian="little")

  #file_header_ver
  file_header_ver <- readBin(con, what="double", 1, size=4, endian="little")

  ##jump
  stepping <- readBin(con, what="raw", 1000, size=1, endian="little")

  ##WinView_id - set to 19,088,743 (or 1234567 hex) (required for legacy reasons)
  WinView_id <- readBin(con, what="integer", 1, size=4, endian="little", signed = TRUE)

  ##jump
  stepping <- readBin(con, what="raw", 1098, size=1, endian="little")

  ##lastvalue - set to 21,845 (or 5555 hex) (required for legacy reasons)
  lastvalue <- readBin(con, what="integer", 1, size=2, endian="little", signed = TRUE)


  ##end header
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ##create info element list from data
  temp.info <- list(ControllerVersion,
                    LogicOutput,
                    AmpHiCapLowNoise,
                    xDimDet, yDimDet,
                    xdim, ydim,
                    VChipXdim, VChipYdim,
                    Date,
                    noscan,
                    mode,  exp_sec,
                    DetTemperature,
                    DetType,
                    datatype,
                    scramble,
                    lnoscan,
                    lavgexp,
                    ReadoutTime,
                    TriggeredModeFlag,
                    NumFrames,
                    file_header_ver)

  ##set name for list elements
  names(temp.info) <- c("ControllerVersion", "LogicOutput", "AmpHiCapLowNoise", "xDimDet", "yDimDet",
                        "xdim", "ydim", "VChipXdim", "VChipYdim", "Date", "noscan", "mode", "exp_sec",
                        "DetTemperature", "DetType", "datatype", "scramble", "lnoscan", "lavgexp",
                        "ReadoutTime", "TriggeredModeFlag", "NumFrames", "file_header_ver")

  # read count value data ---------------------------------------------------
  ##set functions

  ## define the reading function according to the datatype
  if (!datatype %in% c(0, 1, 2, 3, 8)) {
    .throw_error("Unknown 'datatype'") # nocov
  }
  what <- if (datatype == 0) "double" else "integer"
  size <- if (datatype %in% 2:3) 2 else 4
  sign <- if (datatype %in% 0:2) TRUE else FALSE
  read.data <- function(n.counts){
    readBin(con, what = what, n = n.counts, size = size, signed = sign,
            endian = "little")
  }

  ##loop over all frames
  ##output
  if(verbose)
    cat("\n[read_SPE2R()]\n\t >>", file)

  ##set progressbar
  if (txtProgressBar) {
    pb<-txtProgressBar(min=0,max=diff(frame.range)+1, char="=", style=3)
  }

  ##stepping for frame range
  temp <- readBin(con, what = "raw", (min(frame.range)-1)*2, size = 1, endian = "little")

  for(i in 1:(diff(frame.range)+1)){#NumFrames
    temp.data <- matrix(read.data(n.counts = (xdim * ydim)),
                        ncol = ydim,
                        nrow = xdim)

    if(exists("data.list") == FALSE){

      data.list <- list(temp.data)

    }else{

      data.list <- c(data.list, list(temp.data))
    }

    ##update progress bar
    if (txtProgressBar) {
      setTxtProgressBar(pb, i)
    }
  }

  ##close
  if (txtProgressBar) {
    close(pb)
    cat("\t >>", i,"records have been read successfully!\n\n")
  }

  # Output ------------------------------------------------------------------

  if(output.object == "RLum.Data.Spectrum" | output.object == "matrix"){
    ##to create a spectrum object the matrix has to transposed and
    ##the row sums are needed

    data.spectrum.vector <- sapply(1:length(data.list), function(x){
      rowSums(data.list[[x]])
    })

    ##split vector to matrix
    data.spectrum.matrix <- matrix(data.spectrum.vector,
                                   nrow = xdim,
                                   ncol = length(data.list))

    ##set column and row names
    colnames(data.spectrum.matrix) <- as.character(1:ncol(data.spectrum.matrix))
    rownames(data.spectrum.matrix) <- as.character(1:nrow(data.spectrum.matrix))


    ##set output object
    object <- set_RLum(
      class = "RLum.Data.Spectrum",
      originator = "read_SPE2R",
      recordType = "Spectrum",
      curveType = "measured",
      data = data.spectrum.matrix,
      info = temp.info)

    ##optional matrix object
    if (output.object == "matrix") {
      object <- get_RLum(object)
    }


  }else if(output.object == "RLum.Data.Image"){
    object <- as(data.list, "RLum.Data.Image")
    object@originator <- "read_SPE2R"
    object@recordType = "Image"
    object@curveType <- "measured"
    object@info <- temp.info
  }

  ##close con
  close(con)

  ##return values
  return(object)
}

#' Convert portable OSL data to an Risoe.BINfileData object
#'
#' Converts an \code{RLum.Analysis} object produced by the function \code{read_PSL2R()} to
#' an \code{Risoe.BINfileData} object \bold{(BETA)}.
#'
#' This function converts an \code{\linkS4class{RLum.Analysis}} object that was produced
#' by the \code{\link{read_PSL2R}} function to an \code{\linkS4class{Risoe.BINfileData}}.
#' The \code{Risoe.BINfileData} can be used to write a Risoe BIN file via
#' \code{\link{write_R2BIN}}.
#'
#' @param object \code{\linkS4class{RLum.Analysis}} (\bold{required}):
#' \code{RLum.Analysis} object produced by \code{\link{read_PSL2R}}
#'
#' @param ... currently not used.
#'
#' @return Returns an S4 \code{\linkS4class{Risoe.BINfileData}} object that can
#' be used to write a BIN file using \code{\link{write_R2BIN}}.
#'
#' @seealso \code{\linkS4class{RLum.Analysis}}, \code{\linkS4class{RLum.Data.Curve}},
#' \code{\linkS4class{Risoe.BINfileData}}
#'
#' @author Christoph Burow, University of Cologne (Germany)
#'
#' @section Function version: 0.0.1
#'
#' @keywords IO
#'
#' @examples
#'
#' # (1) load and plot example data set
#' data("ExampleData.portableOSL", envir = environment())
#' plot_RLum(ExampleData.portableOSL)
#'
#' # (2) merge all RLum.Analysis objects into one
#' merged <- merge_RLum(ExampleData.portableOSL)
#' merged
#'
#' # (3) convert to RisoeBINfile object
#' bin <- PSL2Risoe.BINfileData(merged)
#' bin
#'
#' # (4) write Risoe BIN file
#' \dontrun{
#' write_R2BIN(bin, "~/portableOSL.binx")
#' }
#'
#' @export
PSL2Risoe.BINfileData <- function(object, ...) {

  ## INTEGRITY CHECKS ----
  if (!inherits(object, "RLum.Analysis"))
    stop("Only objects of class 'RLum.Analysis' are allowed.", call. = FALSE)
  if (!all(sapply(object, class) == "RLum.Data.Curve"))
    stop("The 'RLum.Analysis' object must only contain objects of class 'RLum.Data.Curve'.", call. = FALSE)
  if (!all(sapply(object, function(x) x@originator) == "read_PSL2R"))
    stop("Only objects originating from 'read_PSL2R()' are allowed.", call. = FALSE)

  ## EXTRACT CURVE INFORMATION ----
  curves <- get_RLum(object)

  ## COLLECT META INFORMATION ----
  META <- do.call(rbind, lapply(curves, function(x) {

    NPOINTS <- as.integer(x@info$settings$stimulation_time)
    LTYPE <- x@info$settings$stimulation_unit
    COMMENT <- x@info$settings$measurement
    HIGH <- x@info$settings$stimulation_time
    DATE <- format(x@info$settings$Date, format = "%d%m%y")
    TIME <- x@info$settings$Time
    if (nchar(TIME) < 8)
      TIME <- paste0("0", TIME)
    SAMPLE <- x@info$settings$Sample
    FNAME <- x@info$settings$Filename
    SEQUENCE <- strtrim(paste(x@info$settings$Run_Name, x@info$settings$Sample_no), 8)


    return(data.frame(NPOINTS = NPOINTS,
                LTYPE = LTYPE,
                COMMENT = COMMENT,
                HIGH = HIGH,
                DATE = DATE,
                TIME = TIME,
                SAMPLE = SAMPLE,
                FNAME = FNAME,
                SEQUENCE = SEQUENCE))
  }))

  ## SAVE DATA ----
  DATA <- lapply(curves, function(x) {
    as.integer(x@data[ ,2])
  })

  # SAVE METADATA ----
  METADATA <- data.frame(ID = seq(1, length(curves), 1),
                         SEL = rep(TRUE, length(curves)),
                         VERSION = rep(7, length(curves)),
                         LENGTH = 447 + 4 * META$NPOINTS,
                         PREVIOUS = 447 + 4 * META$NPOINTS,
                         NPOINTS = META$NPOINTS,
                         RUN = seq(1, length(curves), 1),
                         SET = rep(1, length(curves)),
                         POSITION = rep(1, length(curves)),
                         GRAIN = rep(0, length(curves)),
                         GRAINNUMBER = rep(0, length(curves)),
                         CURVENO = rep(0, length(curves)),
                         XCOORD = rep(0, length(curves)),
                         YCOORD = rep(0, length(curves)),
                         SAMPLE = META$SAMPLE,
                         COMMENT = META$COMMENT,
                         SYSTEMID = rep(0, length(curves)),
                         FNAME = META$FNAME,
                         USER = rep("RLum", length(curves)),
                         TIME = META$TIME,
                         DATE = META$DATE,
                         DTYPE = rep("Natural", length(curves)),
                         BL_TIME = rep(0, length(curves)),
                         BL_UNIT = rep(0, length(curves)),
                         NORM1 = rep(0, length(curves)),
                         NORM2 = rep(0, length(curves)),
                         NORM3 = rep(0, length(curves)),
                         BG = rep(0, length(curves)),
                         SHIFT = rep(0, length(curves)),
                         TAG = rep(1, length(curves)),
                         LTYPE = META$LTYPE,
                         LIGHTSOURCE = rep("None", length(curves)),
                         LPOWER = rep(100, length(curves)),
                         LIGHTPOWER = rep(100, length(curves)),
                         LOW = rep(0, length(curves)),
                         HIGH = META$HIGH,
                         RATE = rep(0, length(curves)),
                         TEMPERATURE = rep(0, length(curves)),
                         MEASTEMP = rep(0, length(curves)),
                         AN_TEMP = rep(0, length(curves)),
                         AN_TIME = rep(0, length(curves)),
                         TOLDELAY = rep(0, length(curves)),
                         TOLON = rep(0, length(curves)),
                         TOLOFF = rep(0, length(curves)),
                         IRR_TIME = rep(0, length(curves)),
                         IRR_TYPE = rep(0L, length(curves)),
                         IRR_UNIT = rep(0, length(curves)),
                         IRR_DOSERATE = rep(0, length(curves)),
                         IRR_DOSERATEERR = rep(0, length(curves)),
                         TIMESINCEIRR = rep(-1, length(curves)),
                         TIMETICK = rep(1e-07, length(curves)),
                         ONTIME = rep(0, length(curves)),
                         OFFTIME = rep(NA, length(curves)),
                         STIMPERIOD = rep(0, length(curves)),
                         GATE_ENABLED = rep(0, length(curves)),
                         ENABLE_FLAGS = rep(0, length(curves)),
                         GATE_START = rep(0, length(curves)),
                         GATE_STOP = rep(0, length(curves)),
                         PTENABLED = rep(0, length(curves)),
                         DTENABLED = rep(0, length(curves)),
                         DEADTIME = rep(0, length(curves)),
                         MAXLPOWER = rep(0, length(curves)),
                         XRF_ACQTIME = rep(0, length(curves)),
                         XRF_HV = rep(0, length(curves)),
                         XRF_CURR = rep(0, length(curves)),
                         XRF_DEADTIMEF = rep(0, length(curves)),
                         SEQUENCE = META$SEQUENCE,
                         DETECTOR_ID = rep(NA, length(curves)),
                         LOWERFILTER_ID = rep(NA, length(curves)),
                         UPPERFILTER_ID = rep(NA, length(curves)),
                         ENOISEFACTOR = rep(NA, length(curves)),
                         MARKPOS_X1 = rep(0, length(curves)),
                         MARKPOS_Y1 = rep(0, length(curves)),
                         MARKPOS_X2 = rep(0, length(curves)),
                         MARKPOS_Y2 = rep(0, length(curves)),
                         MARKPOS_X3 = rep(0, length(curves)),
                         MARKPOS_Y3 = rep(0, length(curves)),
                         EXTR_START = rep(0, length(curves)),
                         EXTR_END = rep(0, length(curves)),
                         RECTYPE = rep(0, length(curves)))

  ## CREATE Risoe.BINfileData OBJECT ----
  bin <- set_Risoe.BINfileData(METADATA = METADATA,
                               DATA = DATA,
                               .RESERVED = list())


  ## RETURN VALUE ----
  return(bin)
}




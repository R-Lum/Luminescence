#' Convert portable OSL data to an Risoe.BINfileData object
#'
#' Converts an RLum.Analysis object produced by the function 'read_PSL2R()' to
#' an Risoe.BINfileData object 
#'
#' <placeholder>
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
#'
#' @author Christoph Burow, University of Cologne (Germany)
#'
#' @section Function version: 0.0.1
#'
#' @keywords IO
#' 
#' @examples
#' # none available yet
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
  NPOINTS <- sapply(curves, function(x) {
    as.integer(x@info$settings$stimulation_time)
  })
  LTYPE <- sapply(curves, function(x) {
    x@info$settings$stimulation_unit
  })
  COMMENT <- sapply(curves, function(x) {
    x@info$settings$measurement
  })
  HIGH <- sapply(curves, function(x) {
    x@info$settings$stimulation_time
  })
  
  ## SAVE DATA ----
  DATA <- lapply(curves, function(x) {
    as.integer(x@data[ ,2])
  })
  
  # SAVE METADATA ----
  METADATA <- data.frame(ID = seq(1, length(curves), 1),
                         SEL = rep(TRUE, length(curves)),
                         VERSION = rep(7, length(curves)),
                         LENGTH = 447 + 4 * NPOINTS,
                         PREVIOUS = 447 + 4 * NPOINTS,
                         NPOINTS = NPOINTS,
                         RUN = seq(1, length(curves), 1),
                         SET = rep(1, length(curves)),
                         POSITION = rep(1, length(curves)),
                         GRAIN = rep(0, length(curves)),
                         GRAINNUMBER = rep(0, length(curves)),
                         CURVENO = rep(0, length(curves)),
                         XCOORD = rep(0, length(curves)),
                         YCOORD = rep(0, length(curves)),
                         SAMPLE = rep(paste(object@info$Run_Name, object@info$Sample_no), length(curves)),
                         COMMENT = COMMENT,
                         SYSTEMID = rep(0, length(curves)),
                         FNAME = rep(object@info$Filename, length(curves)),
                         USER = rep("RLum", length(curves)),
                         TIME = rep(format(Sys.time(), "%H:%M:%S"), length(curves)),
                         DATE = rep(format(Sys.Date()+10, "%d%m%y"), length(curves)),
                         DTYPE = rep("Natural", length(curves)),
                         BL_TIME = rep(0, length(curves)),
                         BL_UNIT = rep(0, length(curves)),
                         NORM1 = rep(0, length(curves)),
                         NORM2 = rep(0, length(curves)),
                         NORM3 = rep(0, length(curves)),
                         BG = rep(0, length(curves)),
                         SHIFT = rep(0, length(curves)),
                         TAG = rep(1, length(curves)),
                         LTYPE = LTYPE,
                         LIGHTSOURCE = rep("None", length(curves)),
                         LPOWER = rep(100, length(curves)),
                         LIGHTPOWER = rep(100, length(curves)),
                         LOW = rep(0, length(curves)),
                         HIGH = HIGH,
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
                         DETECTOR_ID = rep(NA, length(curves)),
                         LOWERFILTER_ID = rep(NA, length(curves)),
                         UPPERFILTER_ID = rep(NA, length(curves)),
                         ENOISEFACTOR = rep(NA, length(curves)),
                         SEQUENCE = rep(NA, length(curves)))
  
  ## CREATE Risoe.BINfileData OBJECT ----
  bin <- set_Risoe.BINfileData(METADATA = METADATA,
                               DATA = DATA, 
                               .RESERVED = list())
  
  
  ## RETURN VALUE ----
  return(bin)
}




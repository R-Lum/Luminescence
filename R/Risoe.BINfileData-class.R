#' @include get_Risoe.BINfileData.R set_Risoe.BINfileData.R
NULL

#' Class `"Risoe.BINfileData"`
#'
#' S4 class object for luminescence data in R. The object is produced as output
#' of the function [read_BIN2R].
#'
#'
#'
#' @name Risoe.BINfileData-class
#'
#' @docType class
#'
#' @slot METADATA Object of class "data.frame" containing the meta information for each curve.
#'
#' @slot DATA Object of class "list" containing numeric vector with count data.
#'
#' @slot .RESERVED Object of class "list" containing list of undocumented raw values for internal use only.
#'
#' @keywords internal
#'
#' @note
#'
#' **Internal METADATA - object structure**
#'
#' This structure is compatible with BIN/BINX-files version 03-08, however, it does not follow (in its
#' sequential arrangement) the manual provided by the manufacturer,
#' but an own structure accounting for the different versions.
#'
#' \tabular{rllll}{
#' **#** \tab **Name** \tab **Data Type** \tab **V** \tab **Description** \cr
#' `[,1]` \tab `ID`  \tab `numeric` \tab RLum \tab Unique record ID (same ID as in slot `DATA`)\cr
#' `[,2]` \tab `SEL` \tab `logic` \tab RLum \tab Record selection, not part official BIN-format, triggered by TAG\cr
#' `[,3]` \tab `VERSION` \tab `raw` \tab 03-08 \tab BIN-file version number \cr
#' `[,4]` \tab `LENGTH` \tab `integer` \tab 03-08 \tab Length of this record\cr
#' `[,5]` \tab `PREVIOUS` \tab `integer` \tab 03-08 \tab Length of previous record\cr
#' `[,6]` \tab `NPOINTS` \tab `integer` \tab 03-08 \tab Number of data points in the record\cr
#' `[,7]` \tab `RECTYPE` \tab `integer` \tab 08 \tab Record type \cr
#' `[,8]` \tab `RUN` \tab `integer` \tab 03-08 \tab Run number\cr
#' `[,9]` \tab `SET` \tab `integer` \tab 03-08 \tab Set number\cr
#' `[,10]`  \tab `POSITION` \tab  `integer` \tab 03-08 \tab Position number\cr
#' `[,11]` \tab `GRAIN` \tab `integer` \tab 03-04 \tab Grain number\cr
#' `[,12]` \tab `GRAINNUMBER` \tab `integer` \tab 05-08 \tab Grain number\cr
#' `[,13]` \tab `CURVENO` \tab `integer` \tab 05-08 \tab Curve number\cr
#' `[,14]` \tab `XCOORD` \tab `integer` \tab 03-08 \tab X position of a single grain\cr
#' `[,15]` \tab `YCOORD` \tab `integer` \tab 03-08 \tab Y position of a single grain\cr
#' `[,16]` \tab `SAMPLE` \tab `factor` \tab 03-08 \tab Sample name\cr
#' `[,17]` \tab `COMMENT` \tab `factor` \tab 03-08 \tab Comment name\cr
#' `[,18]` \tab `SYSTEMID` \tab `integer` \tab 03-08 \tab Risø system id\cr
#' `[,19]` \tab `FNAME` \tab `factor` \tab 05-08 \tab File name (*.bin/*.binx)\cr
#' `[,20]` \tab `USER` \tab `factor` \tab 03-08 \tab User name\cr
#' `[,21]` \tab `TIME` \tab `character` \tab 03-08 \tab Data collection time (`hh-mm-ss`)\cr
#' `[,22]` \tab `DATE` \tab `factor` \tab 03-08 \tab Data collection date (`ddmmyy`)\cr
#' `[,23]` \tab `DTYPE` \tab `character` \tab 03-08 \tab Data type\cr
#' `[,24]` \tab `BL_TIME` \tab `numeric` \tab 03-08 \tab Bleaching time\cr
#' `[,25]` \tab `BL_UNIT` \tab `integer` \tab 03-08 \tab Bleaching unit (mJ, J, s, min, h)\cr
#' `[,26]` \tab `NORM1` \tab `numeric` \tab 03-08 \tab Normalisation factor (1)\cr
#' `[,27]` \tab `NORM2` \tab `numeric` \tab 03-08 \tab Normalisation factor (2)\cr
#' `[,28]` \tab `NORM3` \tab `numeric` \tab 03-08 \tab Normalisation factor (3)\cr
#' `[,29]` \tab `BG` \tab `numeric` \tab 03-08 \tab Background level\cr
#' `[,30]` \tab `SHIFT` \tab `integer` \tab 03-08 \tab Number of channels to shift data\cr
#' `[,31]` \tab `TAG` \tab `integer` \tab 03-08 \tab Tag, triggers `SEL`\cr
#' `[,32]` \tab `LTYPE` \tab `character` \tab 03-08 \tab Luminescence type\cr
#' `[,33]` \tab `LIGHTSOURCE` \tab `character` \tab 03-08 \tab Light source\cr
#' `[,34]` \tab `LPOWER` \tab `numeric` \tab 03-08 \tab Optical stimulation power\cr
#' `[,35]` \tab `LIGHTPOWER` \tab `numeric` \tab 05-08 \tab Optical stimulation power\cr
#' `[,36]` \tab `LOW` \tab `numeric` \tab 03-08 \tab Low (temperature, time, wavelength)\cr
#' `[,37]` \tab `HIGH` \tab `numeric` \tab 03-08 \tab High (temperature, time, wavelength)\cr
#' `[,38]` \tab `RATE` \tab `numeric` \tab 03-08 \tab Rate (heating rate, scan rate)\cr
#' `[,39]` \tab `TEMPERATURE` \tab `integer` \tab 03-08 \tab Sample temperature\cr
#' `[,40]` \tab `MEASTEMP` \tab `integer` \tab 05-08 \tab Measured temperature\cr
#' `[,41]` \tab `AN_TEMP` \tab `numeric` \tab 03-08 \tab Annealing temperature\cr
#' `[,42]` \tab `AN_TIME` \tab `numeric` \tab 03-08 \tab Annealing time\cr
#' `[,43]` \tab `TOLDELAY` \tab `integer` \tab 03-08 \tab TOL 'delay' channels\cr
#' `[,44]` \tab `TOLON` \tab `integer` \tab 03-08 \tab TOL 'on' channels\cr
#' `[,45]` \tab `TOLOFF` \tab `integer` \tab 03-08 \tab TOL 'off' channels\cr
#' `[,46]` \tab `IRR_TIME` \tab `numeric` \tab 03-08 \tab Irradiation time\cr
#' `[,47]` \tab `IRR_TYPE` \tab `integer` \tab 03-08 \tab Irradiation type (alpha, beta or gamma)\cr
#' `[,48]` \tab `IRR_UNIT` \tab `integer` \tab 03-04 \tab Irradiation unit (Gy, rad, s, min, h)\cr
#' `[,49]` \tab `IRR_DOSERATE` \tab `numeric` \tab 05-08 \tab Irradiation dose rate (Gy/s)\cr
#' `[,50]` \tab `IRR_DOSERATEERR` \tab `numeric` \tab 06-08 \tab Irradiation dose rate error (Gy/s)\cr
#' `[,51]` \tab `TIMESINCEIRR` \tab `integer` \tab 05-08 \tab Time since irradiation (s)\cr
#' `[,52]` \tab `TIMETICK` \tab `numeric` \tab 05-08 \tab Time tick for pulsing (s)\cr
#' `[,53]` \tab `ONTIME` \tab `integer` \tab 05-08 \tab On-time for pulsing (in time ticks)\cr
#' `[,54]` \tab `OFFTIME` \tab `integer` \tab 03 \tab Off-time for pulsed stimulation (in s) \cr
#' `[,55]` \tab `STIMPERIOD` \tab `integer` \tab 05-08 \tab Stimulation period (on+off in time ticks)\cr
#' `[,56]` \tab `GATE_ENABLED` \tab `raw` \tab 05-08 \tab PMT signal gating enabled\cr
#' `[,57]` \tab `ENABLE_FLAGS` \tab `raw` \tab 05-08 \tab PMT signal gating  enabled\cr
#' `[,58]` \tab `GATE_START` \tab `integer` \tab 05-08 \tab Start gating (in time ticks)\cr
#' `[,59]` \tab `GATE_STOP` \tab `integer` \tab 05-08 \tab Stop gating (in time ticks), `'Gateend'` for version 04, here only GATE_STOP is used\cr
#' `[,60]` \tab `PTENABLED` \tab `raw` \tab 05-08 \tab Photon time enabled\cr
#' `[,61]` \tab `DTENABLED` \tab `raw` \tab 05-08 \tab PMT dead time correction enabled\cr
#' `[,62]` \tab `DEADTIME` \tab `numeric` \tab 05-08 \tab PMT dead time (s)\cr
#' `[,63]` \tab `MAXLPOWER` \tab `numeric` \tab 05-08 \tab Stimulation power to 100 percent (mW/cm^2)\cr
#' `[,64]` \tab `XRF_ACQTIME` \tab `numeric` \tab 05-08 \tab XRF acquisition time (s)\cr
#' `[,65]` \tab `XRF_HV` \tab `numeric` \tab 05-08 \tab XRF X-ray high voltage (V)\cr
#' `[,66]` \tab `XRF_CURR` \tab `integer` \tab 05-08 \tab XRF X-ray current (µA)\cr
#' `[,67]` \tab `XRF_DEADTIMEF` \tab `numeric` \tab 05-08 \tab XRF dead time fraction\cr
#' `[,68]` \tab `DETECTOR_ID` \tab `raw` \tab 07-08 \tab Detector ID\cr
#' `[,69]` \tab `LOWERFILTER_ID` \tab `integer` \tab 07-08 \tab Lower filter ID in reader\cr
#' `[,70]` \tab `UPPERFILTER_ID` \tab `integer` \tab 07-08 \tab Upper filter ID in reader\cr
#' `[,71]` \tab `ENOISEFACTOR` \tab `numeric` \tab 07-08 \tab Excess noise filter, usage unknown \cr
#' `[,72]` \tab `MARKPOS_X1` \tab `numeric` \tab 08 \tab Coordinates marker position 1 \cr
#' `[,73]` \tab `MARKPOS_Y1` \tab `numeric` \tab 08 \tab Coordinates marker position 1 \cr
#' `[,74]` \tab `MARKPOS_X2` \tab `numeric` \tab 08 \tab Coordinates marker position 2 \cr
#' `[,75]` \tab `MARKPOS_Y2` \tab `numeric` \tab 08 \tab Coordinates marker position 2 \cr
#' `[,76]` \tab `MARKPOS_X3` \tab `numeric` \tab 08 \tab Coordinates marker position 3 \cr
#' `[,77]` \tab `MARKPOS_Y3` \tab `numeric` \tab 08 \tab Coordinates marker position 3 \cr
#' `[,78]` \tab `EXTR_START` \tab `numeric` \tab 08 \tab usage unknown \cr
#' `[,79]` \tab `EXTR_END` \tab `numeric` \tab 08 \tab usage unknown\cr
#' `[,80]` \tab `SEQUENCE` \tab `character` \tab 03-04 \tab Sequence name
#' }
#' V = BIN-file version (RLum means that it does not depend on a specific BIN version)
#'
#' Note that the `Risoe.BINfileData` object combines all values from
#' different versions from the BIN-file, reserved bits are skipped, however,
#' the function [write_R2BIN] reset arbitrary reserved bits. Invalid
#' values for a specific version are set to `NA`. Furthermore, the
#' internal R data types do not necessarily match the required data types for
#' the BIN-file data import! Data types are converted during data import.\cr
#'
#' **LTYPE** values
#'
#' \tabular{rll}{
#'  VALUE \tab TYPE \tab DESCRIPTION \cr
#' `[0]` \tab `TL` \tab: Thermoluminescence \cr
#' `[1]` \tab `OSL` \tab: Optically stimulated luminescence \cr
#' `[2]` \tab `IRSL` \tab: Infrared stimulated luminescence \cr
#' `[3]` \tab `M-IR` \tab: Infrared monochromator scan\cr
#' `[4]` \tab `M-VIS` \tab: Visible monochromator scan\cr
#' `[5]` \tab `TOL` \tab: Thermo-optical luminescence \cr
#' `[6]` \tab `TRPOSL` \tab: Time Resolved Pulsed OSL\cr
#' `[7]` \tab `RIR` \tab: Ramped IRSL\cr
#' `[8]` \tab `RBR` \tab: Ramped (Blue) LEDs\cr
#' `[9]` \tab `USER` \tab: User defined\cr
#' `[10]` \tab `POSL` \tab: Pulsed OSL \cr
#' `[11]` \tab `SGOSL` \tab: Single Grain OSL\cr
#' `[12]` \tab `RL` \tab: Radio Luminescence \cr
#' `[13]` \tab `XRF` \tab: X-ray Fluorescence
#' }
#'
#' **DTYPE** values
#'
#' \tabular{rl}{
#' VALUE \tab DESCRIPTION \cr
#' `[0]` \tab Natural \cr
#' `[1]` \tab N+dose \cr
#' `[2]` \tab Bleach \cr
#' `[3]` \tab Bleach+dose \cr
#' `[4]` \tab Natural (Bleach) \cr
#' `[5]` \tab N+dose (Bleach) \cr
#' `[6]` \tab Dose \cr
#' `[7]` \tab Background
#' }
#'
#' **LIGHTSOURCE** values
#'
#' \tabular{rl}{
#'  VALUE \tab DESCRIPTION \cr
#' `[0]` \tab None \cr
#' `[1]` \tab Lamp \cr
#' `[2]` \tab IR diodes/IR Laser \cr
#' `[3]` \tab Calibration LED \cr
#' `[4]` \tab Blue Diodes \cr
#' `[5]` \tab White light \cr
#' `[6]` \tab Green laser (single grain) \cr
#' `[7]` \tab IR laser (single grain) }
#'
#' **Internal DATA - object structure**
#'
#' With version 8 of the BIN/BINX file format, slot `@DATA` (byte array `DPOINTS`) can
#' contain two different values:
#'
#' 1. `DPOINTS` (standard for `RECTYPE` := (0,1)): is a vector with the length defined
#' through `NPOINTS`. This is the standard for xy-curves since version 03.
#'
#' 2. `DPOINTS` (`RECTYPE` := 128) is contains no count values but information about
#' the definition of the regions of interest (ROI). Each definition is 504 bytes long.
#' The number of definitions is defined by `NPOINTS` in `@METADATA`. The record
#' describes basically the geometric features of the regions of interest.
#' The representation in R is a nested [list].
#'
#' \tabular{rllll}{
#' **#** \tab **Name** \tab **Data Type** \tab **V** \tab **Description** \cr
#'`[,1]` \tab `NOFPOINTS`  \tab `numeric` \tab 08 \tab number of points in the definition (e.g., if the ROI is a rectangle: 4)\cr
#'`[,2]` \tab `USEDFOR`  \tab `logical` \tab 08 \tab samples for which the ROI is used for; a maximum of 48 samples are allowed.\cr
#'`[,3]` \tab `SHOWNFOR`  \tab `logical` \tab 08 \tab samples for which the ROI is shown for; a maximum of 48 samples are allowed.\cr
#'`[,4]` \tab `COLOR`  \tab `numeric` \tab 08 \tab The colour values of the ROI.\cr
#'`[,5]` \tab `X`  \tab `numeric` \tab 08 \tab The x coordinates used to draw the ROI geometry (up to 50 points are allowed).\cr
#'`[,6]` \tab `Y`  \tab `numeric` \tab 08 \tab The y coordinates used to draw the ROI geometry (up to 50 points are allowed).\cr
#' }
#'
#' (information on the BIN/BINX file format are kindly provided by Risø, DTU Nutech)
#'
#' @section Objects from the Class: Objects can be created by calls of the form
#' `new("Risoe.BINfileData", ...)`.
#'
#' @section Function version: 0.4.1
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)\cr
#' based on information provided by Torben Lapp and Karsten Bracht Nielsen (Risø DTU, Denmark)
#'
#' @seealso [plot_Risoe.BINfileData], [read_BIN2R], [write_R2BIN],
#' [merge_Risoe.BINfileData], [Risoe.BINfileData2RLum.Analysis]
#'
#' @references
#' Risø DTU, 2013. The Sequence Editor User Manual - Feb 2013 and Risø DTU, 2016.
#'
#' The Sequence Editor User Manual - February 2016
#'
#' [https://www.fysik.dtu.dk]()
#'
#' @keywords classes
#'
#' @examples
#'
#' showClass("Risoe.BINfileData")
#'
#' @md
#' @export
setClass("Risoe.BINfileData",
         slots = list(
           METADATA = "data.frame",
           DATA = "list",
           .RESERVED = "list"
           ),
         prototype = prototype(
           METADATA = data.frame(
             ID = integer(),
             SEL = logical(),
             VERSION = integer(),
             LENGTH = integer(),
             PREVIOUS = integer(),
             NPOINTS = integer(),
             RECTYPE = integer(),
             RUN = integer(),
             SET = integer(),
             POSITION = integer(),
             GRAIN = integer(),
             GRAINNUMBER = integer(),
             CURVENO = integer(),
             XCOORD = integer(),
             YCOORD = integer(),
             SAMPLE = character(),
             COMMENT = character(),
             SYSTEMID = integer(),
             FNAME = character(),
             USER = character(),
             TIME = character(),
             DATE = character(),
             DTYPE = character(),
             BL_TIME = numeric(),
             BL_UNIT = integer(),
             NORM1 = numeric(),
             NORM2 = numeric(),
             NORM3 = numeric(),
             BG = numeric(),
             SHIFT = integer(),
             TAG = integer(),
             LTYPE = character(),
             LIGHTSOURCE = character(),
             LPOWER = numeric(),
             LIGHTPOWER = numeric(),
             LOW = numeric(),
             HIGH = numeric(),
             RATE = numeric(),
             TEMPERATURE = numeric(),
             MEASTEMP = numeric(),
             AN_TEMP = numeric(),
             AN_TIME = numeric(),
             TOLDELAY = integer(),
             TOLON = integer(),
             TOLOFF = integer(),
             IRR_TIME = numeric(),
             IRR_TYPE = integer(),
             IRR_UNIT = integer(),
             IRR_DOSERATE = numeric(),
             IRR_DOSERATEERR = numeric(),
             TIMESINCEIRR = numeric(),
             TIMETICK = numeric(),
             ONTIME = numeric(),
             OFFTIME = numeric(),
             STIMPERIOD = integer(),
             GATE_ENABLED = numeric(),
             ENABLE_FLAGS = numeric(),
             GATE_START = numeric(),
             GATE_STOP = numeric(),
             PTENABLED = numeric(),
             DTENABLED = numeric(),
             DEADTIME = numeric(),
             MAXLPOWER = numeric(),
             XRF_ACQTIME = numeric(),
             XRF_HV = numeric(),
             XRF_CURR = numeric(),
             XRF_DEADTIMEF = numeric(),
             DETECTOR_ID = integer(),
             LOWERFILTER_ID = integer(),
             UPPERFILTER_ID = integer(),
             ENOISEFACTOR = numeric(),
             MARKPOS_X1 = numeric(),
             MARKPOS_Y1 = numeric(),
             MARKPOS_X2 = numeric(),
             MARKPOS_Y2 = numeric(),
             MARKPOS_X3 = numeric(),
             MARKPOS_Y3 = numeric(),
             EXTR_START = numeric(),
             EXTR_END = numeric(),
             SEQUENCE = character(),
             stringsAsFactors=FALSE
           ),
           DATA = list(),
           .RESERVED = list()
          )
         )


# show method --------
#' @describeIn Risoe.BINfileData
#' Show structure of RLum and Risoe.BINfile class objects
#'
#' @md
#' @export
setMethod(f = "show",
          signature = signature(object = "Risoe.BINfileData"),
          definition = function(object){

            if(nrow(object@METADATA) != 0){
              ## check if image/ROI data are present; get ID and remove information
              if(!is.null(object@METADATA[["RECTYPE"]]))
                id_128 <- object@METADATA[["RECTYPE"]] != 128
              else
                id_128 <- rep(TRUE, nrow(object@METADATA))

              version <- suppressWarnings(paste(unique(object@METADATA[id_128,"VERSION"]), collapse = ", "))
              systemID <- suppressWarnings(paste(unique(object@METADATA[id_128,"SYSTEMID"]), collapse = ", "))
              filename <- as.character(object@METADATA[1,"FNAME"])
              records.overall <- length(object@DATA)
              records.type <- table(object@METADATA[id_128,"LTYPE"])
              user <- paste(unique(as.character(object@METADATA[id_128,"USER"])), collapse = ", ")
              date <- paste(unique(as.character(object@METADATA[id_128,"DATE"])), collapse = ", ")
              run.range <- suppressWarnings(range(object@METADATA[id_128,"RUN"]))
              set.range <- suppressWarnings(range(object@METADATA[id_128,"SET"]))
              grain.range <- suppressWarnings(range(object@METADATA[id_128,"GRAIN"]))

              pos.range <- suppressWarnings(range(object@METADATA[id_128,"POSITION"]))

              records.type.count <- vapply(seq_along(records.type), function(x){
                paste0(names(records.type[x]),"\t(n = ", records.type[x],")")
               }, character(1))

              records.type.count <- paste(records.type.count,
                                          collapse="\n\t                      ")

              ##print
              cat("\n[Risoe.BINfileData object]")
              cat("\n\n\tBIN/BINX version:    ", version)
              if(version >= 6){
                cat("\n\tFile name:           ", filename)
              }
              cat("\n\tObject date:         ", date)
              cat("\n\tUser:                ", user)
              cat("\n\tSystem ID:           ", ifelse(systemID == 0,"0 (unknown)", systemID))
              cat("\n\tOverall records:     ", records.overall)
              cat("\n\tRecords type:        ", records.type.count)
              cat("\n\tPosition range:      ", pos.range[1],":",pos.range[2])
                if(max(grain.range) > 0)
                  cat("\n\tGrain range:         ", grain.range[1],":",grain.range[2])

              cat("\n\tRun range:           ", run.range[1],":",run.range[2])

              ## if id_128
              if(any(!id_128))
                cat("\n\t + additional ROI data found in record(s):", paste(which(!id_128), collapse = ", "))

            }else{
              cat("\n[Risoe.BINfileData object]")
              cat("\n\n >> This object is empty!<<")

             }
          }#end function
          )#end setMethod


# set method for object class -----------------------------------

#' @describeIn Risoe.BINfileData
#' The Risoe.BINfileData is normally produced as output of the function read_BIN2R.
#' This construction method is intended for internal usage only.
#'
#' @param METADATA Object of class "data.frame" containing the meta information
#' for each curve.
#'
#' @param DATA Object of class "list" containing numeric vector with count data.
#'
#' @param .RESERVED Object of class "list" containing list of undocumented raw
#' values for internal use only.
#'
#' @md
#' @export
setMethod(f = "set_Risoe.BINfileData",
          signature = signature("ANY"),
          definition = function(METADATA, DATA, .RESERVED) {

            if(length(METADATA) == 0){
              new("Risoe.BINfileData")

            }else{
              new(
                "Risoe.BINfileData",
                METADATA = METADATA,
                DATA = DATA,
                .RESERVED = .RESERVED
              )

            }

          })


# get method for object class -----------------------------------

#' @describeIn Risoe.BINfileData
#' Formal get-method for Risoe.BINfileData object. It does not allow accessing
#' the object directly, it is just showing a terminal message.
#'
#' @param object an object of class [Risoe.BINfileData-class]
#'
#' @param ... other arguments that might be passed
#'
#' @md
#' @export
setMethod("get_Risoe.BINfileData",
          signature= "Risoe.BINfileData",
          definition = function(object, ...) {

            cat("[get_Risoe.BINfileData()] No direct access is provided for this object type. Use the function 'Risoe.BINfileData2RLum.Analysis' for object coercing.")

          })##end setMethod

##-------------------------------------------------------------------------------------------------##
##=================================================================================================##

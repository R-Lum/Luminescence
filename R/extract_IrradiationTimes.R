#' @title Extract Irradiation Times from an XSYG-file or `RLum.Analysis` object
#'
#' @description Extracts irradiation times, dose and times since last irradiation, from a
#' Freiberg Instruments XSYG-file. These information can be further used to
#' update an existing BINX-file.
#'
#' @details
#' The function was written to compensate missing information in the BINX-file
#' output of Freiberg Instruments lexsyg readers. As all information are
#' available within the XSYG-file anyway, these information can be extracted
#' and used for further analysis or/and to stored in a new BINX-file, which can
#' be further used by other software, e.g., *Analyst* (Geoff Duller).
#'
#' Typical application example: *g*-value estimation from fading measurements
#' using the Analyst or any other self-written script.
#'
#' Beside some simple data transformation steps, the function applies
#' functions [read_XSYG2R], [read_BIN2R], [write_R2BIN] for data import and export.
#'
#' **Calculation details**
#'
#' - The value `DURATION.STEP` is calculated as `START` + the end of the time axis for all curves
#' except for `TL`, where the function tries to extract meta information about the duration.
#'
#' - The value `END` is calculated as `START` + `DURACTION.STEP`
#'
#' - All curves for which no prior irradiation was detected receive an `-1` (*Analyst* convention)
#'
#' - Irradiation steps have always `IRR_TIME = 0`
#'
#' @param object [character], [RLum.Analysis-class] or [list] (**required**):
#' path and file name of the XSYG file or an [RLum.Analysis-class]
#' produced by the function [read_XSYG2R];
#' alternatively a `list` of [RLum.Analysis-class] can be provided.
#'
#' **Note**: If an [RLum.Analysis-class] is used, any input for
#' the arguments `file.BINX` and `recordType` will be ignored!
#'
#' @param file.BINX [character] (*optional*):
#' path and file name of an existing BINX-file. If a file name is provided the
#' file will be updated with the information from the XSYG file in the same
#' folder as the original BINX-file.
#'
#' **Note:** The XSYG and the BINX-file must originate from the
#' same measurement!
#'
#' @param recordType [character] (*with default*):
#' select relevant curves types from the XSYG file or [RLum.Analysis-class]
#' object. As the XSYG-file format comprises much more information than usually
#' needed for routine data analysis and allowed in the BINX-file format, only
#' the relevant curves are selected by using the function
#' [get_RLum]. The argument `recordType` works as
#' described for this function.
#'
#' **Note:** A wrong selection will causes a function error. Please change this
#' argument only if you have reasons to do so.
#'
#' @param return_same_as_input [logical] (*with default*): if set to `TRUE` an updated
#' [RLum.Analysis-class] object (or a [list] of it) is returned, with each record having gained two
#' new  info element fields: `IRR_TIME` and `TIMESCINCEIRR`. This makes the [RLum.Analysis-class]
#' object compatible to external functions that search explicitly for `IRR_TIME` and `TIMESCINCEIRR`
#'
#' @param compatibility.mode [logical] (*with default*):
#' this option is parsed only if a BIN/BINX file is produced and it will reset all position
#' values to a max. value of 48, cf.[write_R2BIN]
#'
#' @param txtProgressBar [logical] (*with default*):
#' enable/disable the progress bar during import and export.
#'
#' @note The function can be also used to extract irradiation times from [RLum.Analysis-class] objects
#' previously imported via [read_BIN2R] (`fastForward = TRUE`) or in combination with
#' [Risoe.BINfileData2RLum.Analysis].
#' Unfortunately the timestamp might not be very precise (or even invalid),
#' but it allows to essentially treat different formats in a similar manner.
#'
#' @return
#' An [RLum.Results-class] object is returned with the
#' following structure:
#'
#' ```
#' .. $irr.times (data.frame)
#' ```
#'
#' If `return_same_as_input = TRUE` an [RLum.Analysis-class] or a [list] of it, but
#' we updated info elements including irradiation times.
#'
#' If a BINX-file path and name is set, the output will be additionally
#' transferred into a new BINX-file with the function name as suffix. For the
#' output the path of the input BINX-file itself is used. Note that this will
#' not work if the input object is a file path to an XSYG-file, instead of a
#' link to only one file. In this case the argument input for `file.BINX` is ignored.
#'
#' In the self call mode (input is a `list` of [RLum.Analysis-class] objects
#' a list of [RLum.Results-class] is returned.
#'
#' @note
#' The produced output object contains still the irradiation steps to
#' keep the output transparent. However, for the BINX-file export this steps
#' are removed as the BINX-file format description does not allow irradiations
#' as separate sequences steps.
#'
#' **BINX-file 'Time Since Irradiation' value differs from the table output?**
#'
#' The way the value 'Time Since Irradiation' is defined differs. In the BINX-file the
#' 'Time Since Irradiation' is calculated as the 'Time Since Irradiation' plus the 'Irradiation
#' Time'. The table output returns only the real 'Time Since Irradiation', i.e. time between the
#' end of the irradiation and the next step.
#'
#' **Negative values for `TIMESINCELAST.STEP`?**
#'
#' Yes, this is possible and not a bug, as in the XSYG-file multiple curves are stored for one step.
#' Example: TL step may comprise three curves:
#'
#' - (a) counts vs. time,
#' - (b) measured temperature vs. time and
#' - (c) predefined temperature vs. time.
#'
#' Three curves, but they are all belonging to one TL measurement step, but with regard to
#' the time stamps this could produce negative values as the important function
#' ([read_XSYG2R]) do not change the order of entries for one step
#' towards a correct time order.
#'
#' **`TIMESINCELAST.STEP` is odd if TL curves are involved?**
#'
#' Yes, this is possible! The end time is calculated as start + duration,
#' and the duration is deduced from the time values of each step.
#' A typical TL curve, however, does not have a time but a temperature axis.
#' Hence, the function tries to extract the information from metadata.
#' If that information is missing, the x-values are presumed time values,
#' which might still be wrong.
#'
#' @section Function version: 0.4.0
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Analysis-class], [RLum.Results-class], [Risoe.BINfileData-class],
#' [read_XSYG2R], [read_BIN2R], [write_R2BIN]
#'
#' @references
#' Duller, G.A.T., 2015. The Analyst software package for luminescence data: overview and
#' recent improvements. Ancient TL 33, 35-42. \doi{10.26034/la.atl.2015.489}
#'
#' @keywords IO manip
#'
#' @examples
#' ## take system file
#' xsyg <- system.file("extdata/XSYG_file.xsyg", package="Luminescence")
#'
#' ## the import is automatically
#' ## but you can import it before
#' irr_times <- extract_IrradiationTimes(xsyg)
#' irr_times$irr.times
#'
#' \dontrun{
#' # (1) - example for your own data
#'
#' # set files and run function
#' file.XSYG <- file.choose()
#' file.BINX <- file.choose()
#'
#' extract_IrradiationTimes(file.XSYG = file.XSYG, file.BINX = file.BINX)
#'
#' # export results additionally to a CSV-file in the same directory as the XSYG-file
#' write.table(x = get_RLum(output),
#'  file = paste0(file.BINX,"_extract_IrradiationTimes.csv"),
#'  sep = ";",
#'  row.names = FALSE)
#' }
#'
#' @md
#' @export
extract_IrradiationTimes <- function(
  object,
  file.BINX,
  recordType = c("irradiation (NA)", "IRSL (UVVIS)", "OSL (UVVIS)", "TL (UVVIS)"),
  return_same_as_input = FALSE,
  compatibility.mode = TRUE,
  txtProgressBar = TRUE
) {
  .set_function_name("extract_IrradiationTimes")
  on.exit(.unset_function_name(), add = TRUE)

  # SELF CALL -----------------------------------------------------------------------------------
  if(is.list(object)){
    ##show message for non-supported arguments
    if(!missing(file.BINX))
      .throw_warning("argument 'file.BINX' is not supported in self-call mode.")

    ## expand input arguments
    recordType <- .listify(recordType, length(object))

    ## run function
    results <- lapply(seq_along(object), function(x) {
        extract_IrradiationTimes(
          object = object[[x]],
          recordType = recordType[[x]],
          return_same_as_input = return_same_as_input,
          txtProgressBar = txtProgressBar
        )
      })

    if (length(results) == 0)
      return(NULL)
    return(results)
  }

  ## Integrity checks -------------------------------------------------------
  .validate_class(object, c("character", "RLum.Analysis"),
                  extra = "a 'list' of such objects")
  .validate_not_empty(object)
  .validate_logical_scalar(return_same_as_input)

  if (is.character(object[1])) {
    .validate_length(object, 1)

    ##set object to file.XSYG
    file.XSYG <- object

    ##XSYG
    if (!file.exists(file.XSYG)) {
      .throw_error("Wrong XSYG file name or file does not exist!")
    }
    if (tolower(tools::file_ext(file.XSYG)) != "xsyg") {
      .throw_error("File is expected to have 'xsyg' or 'XSYG' extension")
    }

    ##BINX
    if(!missing(file.BINX)){
      if (!file.exists(file.BINX)) {
        .throw_error("Wrong BINX file name or file does not exist!")
      }
      if (tolower(tools::file_ext(file.BINX)) != "binx") {
        .throw_error("File is expected to have 'binx' or 'BINX' extension")
      }
    }

    # Settings and import XSYG --------------------------------------------------------------------
    temp.XSYG <- read_XSYG2R(
      file = file.XSYG,
      txtProgressBar = txtProgressBar,
      verbose = txtProgressBar)

    if(!missing(file.BINX)){
      temp.BINX <- read_BIN2R(
        file = file.BINX,
        txtProgressBar = txtProgressBar,
        verbose = txtProgressBar)
      temp.BINX.dirname <- dirname(file.XSYG)
    }

    # Some data preparation -----------------------------------------------------------------------
    ##set list
    temp.sequence.list <- list()

    ##select all analysis objects and combine them
    for(i in 1:length(temp.XSYG)){
      ##select sequence and reduce the data set to really wanted values
      temp.sequence.list[[i]] <- get_RLum(
        object = temp.XSYG[[i]]$Sequence.Object,
        recordType = recordType,
        drop = FALSE)

      ##get corresponding position number, this will be needed later on
      temp.sequence.position <- as.numeric(as.character(temp.XSYG[[i]]$Sequence.Header["position",]))
    }

  }else{
    ##now we assume a single RLum.Analysis object
    ##select sequence and reduce the data set to really wanted values, note that no
    ##record selection was made!
    temp.sequence.list <- list(object)
  }

  ## merge objects and restore originator
  temp.sequence <- merge_RLum(temp.sequence.list)
  temp.sequence@originator <- temp.sequence.list[[1]]@originator

# Grep relevant information -------------------------------------------------------------------
  ##Sequence STEP
  STEP <- names_RLum(temp.sequence)

  #START time of each step
  ## we try also to support BIN/BINX files with this function if imported
  ## accordingly
  if(any(temp.sequence@originator %in% c("Risoe.BINfileData2RLum.Analysis", "read_BIN2R"))) {
    temp.START <- vapply(temp.sequence, function(x) {
       paste0(get_RLum(x, info.object = c("DATE")), get_RLum(x, info.object = c("TIME")))
    }, character(1))

    fmt <- if (grepl(":", temp.START[1])) "%y%m%d%H:%M:%S" else "%y%m%d%H%M%S"

  } else {
    temp.START <- vapply(temp.sequence, function(x) {
      tmp <- suppressWarnings(get_RLum(x, info.object = c("startDate")))
      if(is.null(tmp))
        tmp <- as.character(Sys.Date())
      tmp
    }, character(1))

    fmt <- "%Y%m%d%H%M%S"
  }

  ## reformat start date
  START <- strptime(temp.START, format = fmt, tz = "GMT")

  ##DURATION of each STEP
  DURATION.STEP <- vapply(temp.sequence, function(x) {
    ## we treat TL curves differently **if** they have temperature axis
    ## in such case we search for 'duration' that should be there
    if(grepl(pattern = "TL", x@recordType, fixed = TRUE)) {
      t <- as.numeric(x@info$duration)
      if(length(t) != 0)
        return(t)
    }

    ## general fall back
    max(x@data[,1])

  }, numeric(1))

  ##Calculate END time of each STEP
  END <- START + DURATION.STEP

  ##add position number so far an XSYG file was the input
  POSITION <- NA
  if(exists("file.XSYG")){
    POSITION <- rep(temp.sequence.position, each = length_RLum(temp.sequence))

  }else if(!inherits(try(
    suppressWarnings(get_RLum(
      get_RLum(temp.sequence, record.id = 1), info.object = "position")),
    silent = TRUE), "try-error")){

    ##POSITION of each STEP
    POSITION <- vapply(temp.sequence, function(x){
      tmp <- suppressWarnings(get_RLum(x, info.object = c("position")))

      if(is.null(tmp))
        tmp <- get_RLum(x, info.object = c("POSITION"))

      tmp
    }, numeric(1))
  }

  ##Combine the results
  temp.results <- data.frame(POSITION,STEP,START,DURATION.STEP,END)

  # Calculate irradiation duration ------------------------------------------------------------
  if(any(temp.sequence@originator %in% c("Risoe.BINfileData2RLum.Analysis", "read_BIN2R"))) {
    IRR_TIME <- vapply(temp.sequence, function(x) get_RLum(x, info.object = c("IRR_TIME")), numeric(1))

  } else {
    IRR_TIME <- numeric(length = nrow(temp.results))
    temp_last <- 0
    for(i in 1:nrow(temp.results)){
      if(grepl("irradiation", temp.results[["STEP"]][i])) {
        temp_last <- temp.results[["DURATION.STEP"]][i]
        next()
      }

      IRR_TIME[i] <- temp_last
    }
  }

  ## Calculate time since irradiation ---------------------------------------
  time.irr.end <- NA
  TIMESINCEIRR <- vapply(1:nrow(temp.results), function(x) {
    if(grepl("irradiation", temp.results[x,"STEP"])){
      time.irr.end<<-temp.results[x,"END"]
      return(-1)
    }
    if (is.na(time.irr.end))
      return(-1)

    difftime(temp.results[x, "START"], time.irr.end, units = "secs")
  }, numeric(1))

  # Calculate time since last step --------------------------------------------------------------
  TIMESINCELAST.STEP <- vapply(1:nrow(temp.results), function(x){
    if(x == 1)
      return(0)

    difftime(temp.results[x,"START"],temp.results[x-1, "END"], units = "secs")
  }, numeric(1))

  # Combine final results -----------------------------------------------------------------------
  ##results table, export as CSV
  results <- cbind(temp.results,IRR_TIME, TIMESINCEIRR,TIMESINCELAST.STEP)

  # Write BINX-file if wanted -------------------------------------------------------------------
  if(!missing(file.BINX)){
    ##(1) remove all irradiation steps as there is no record in the BINX file and update information
    results.BINX <- results[results[, "STEP"] != "irradiation (NA)", ]

    ## (2) compare entries in the BINX-file with the entries in the table
    ## to make sure that both have the same length
    if(nrow(results.BINX) == nrow(temp.BINX@METADATA)){

      ## (1a) update information on the irradiation time
      temp.BINX@METADATA[["IRR_TIME"]] <- results.BINX[["IRR_TIME"]]

      ## (1b) update information on the time since irradiation by using the
      ## Risoe definition of the parameter, to make the file compatible to
      ## the Analyst
      temp.BINX@METADATA[["TIMESINCEIRR"]] <- results.BINX[["IRR_TIME"]] + results.BINX[["TIMESINCEIRR"]]

      ## update BINX-file
      try <- write_R2BIN(temp.BINX, version = "06",
                         file = paste0(file.BINX,"_extract_IrradiationTimes.BINX"),
                         compatibility.mode = compatibility.mode,
                         verbose = txtProgressBar,
                         txtProgressBar = txtProgressBar)

      ##set message on the format definition
      if(!inherits(x = try, 'try-error')){
        message("[extract_IrradiationTimes()] 'Time Since Irradiation' was redefined in the exported BINX-file to: 'Time Since Irradiation' plus the 'Irradiation Time' to be compatible with the Analyst.")
      }
    } else {
      .throw_message("XSYG-file and BINX-file do not contain similar entries, ",
                     "BINX-file update skipped")
    }
  }

  # Output --------------------------------------------------------------------------------------
   if(return_same_as_input[1]) {
     ## This odd mode we need to ensure that the function supports
     ## correctly all the odd modes with the automated import of XSYG files
     if(exists(x = "temp.XSYG", envir = environment())) {
       object <- lapply(temp.XSYG, function(x) x$Sequence.Object)

        ## make sure that the output is compatible
        if(length(object) == 1)
          object <- object[[1]]

        ## call the function again
        return(
          extract_IrradiationTimes(object, return_same_as_input = TRUE, recordType = recordType))

     }

    object@records <- lapply(seq_along(results$IRR_TIME), function(x){
      add_metadata(object@records[[x]], info_element = "IRR_TIME") <- results$IRR_TIME[[x]]
      add_metadata(object@records[[x]], info_element = "TIMESINCEIRR") <- results$TIMESINCEIRR[[x]]
      object@records[[x]]
    })

    return(object)
   }

  ## regular return
  return(set_RLum(class = "RLum.Results", data = list(irr.times = results)))
}

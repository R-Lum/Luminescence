#' Convert an element from a \linkS4class{TLum.BIN.File} object into a \linkS4class{TLum.Data.Curve} objet
#'
#' This function extract a curve from a \linkS4class{TLum.BIN.File} object and convert it into a \linkS4class{TLum.Data.Curve} objet.
#' The extract element can be identify either by its id or by its position, run and set.
#'
#' @param object
#'  \code{\linkS4class{TLum.BIN.File}} (\bold{required}): object containing the luminescence curves.
#' @param id
#'  \link{integer} (with default): id of the curve.
#' @param pos
#'  \link{integer} (with default): position of the curve.
#' @param run
#'  \link{integer} (with default): run of the curve.
#' @param set
#'  \link{integer} (with default): set of the curve.
#' @param rec_ramp2PH
#'  \link{logical} (with default): Indicate if the signal was record during the ramp up to the preheat temperature.
#' @param rec_duringPH
#'  \link{logical} (with default): Indicate if the signal was record during the preheat plateau.
#'
#' @details
#'  The element that is extracted to be converted into a \linkS4class{TLum.Data.Curve} objet can be identify
#'  either by its id or by its position, run and set.
#'
#' @return
#'  This function return a \linkS4class{TLum.Data.Curve} objet.
#'
#' @export TLum.BIN.File2TLum.Data.Curve
#'

TLum.BIN.File2TLum.Data.Curve <- function(
  object,
  id,
  pos,
  run,
  set,
  rec_duringPH =TRUE,
  rec_ramp2PH =TRUE

){


  # Integrity Check ---------------------------------------------------------

  if (is(object,"TLum.BIN.File")==FALSE){
    stop("[TLum.BIN.File2TLum.Data.Curve] Error: Input object is not of type 'TLum.BIN.File'.")
  }

  if(!is.logical(rec_ramp2PH) || is.na(rec_ramp2PH)){
    stop("[calc_TL.temperature] Error: Input 'rec_ramp2PH' is not of type 'logical'.")
  }

  if(!is.logical(rec_duringPH) || is.na(rec_duringPH)){
    stop("[calc_TL.temperature] Error: Input 'rec_duringPH' is not of type 'logical'.")
  }

  ##if id is set, no input for pos and run is nescessary
  if(missing(id) == TRUE){

    if(missing(pos) == TRUE | missing(run) == TRUE | missing(set) == TRUE){

      temp.missing.arguments <- paste(c(if(missing(pos)==TRUE){"pos"},
                                        if(missing(set)==TRUE){"set"},
                                        if(missing(run)==TRUE){"run"}), collapse=", ")

      stop(paste("[TLum.BIN.File2TLum.Data.Curve] Error: Arguments are missing: ",
                 temp.missing.arguments, ". Or set id.", sep = ""))

    }

    if (!is.numeric(pos)){
      stop("[TLum.BIN.File2TLum.Data.Curve] Error: Argument 'pos' has to be of data type integer.")
    }

    if (!is.numeric(set)){
      stop("[TLum.BIN.File2TLum.Data.Curve] Error: Argument 'set' has to be of data type integer.")
    }

    if (!is.numeric(run)){
      stop("[TLum.BIN.File2TLum.Data.Curve] Error: Argument 'run' has to be of data type integer.")
    }

    # if (length(which(pos/1:48 == 1)) == 0){
    #   stop("[TLum.BIN.File2TLum.Data.Curve] Error: Value for 'pos' out of bounds.")
    # }

    ##get and check valid positions
    positions.valid <- unique(object@METADATA[,"POSITION"])

    if (!(pos %in% positions.valid)){
      stop(paste("[TLum.BIN.File2TLum.Data.Curve] Error: pos = ",pos, " is not valid.
                 Valid positions are: ", paste(as.character(positions.valid), collapse=", "),
                 sep=""))
    }

    ##get and check valid set
    set.valid <- unique(object@METADATA[,"SET"])

    if (!(set %in% set.valid)){
      stop(paste("[TLum.BIN.File2TLum.Data.Curve] Error: set = ",set, " is not valid.
                 Valid values are: ", paste(as.character(set.valid), collapse=", "),
                 sep=""))
    }


    ##get and check valid run
    run.valid <- unique(object@METADATA[,"RUN"])

    if (!(run %in% run.valid)){
      stop(paste("[TLum.BIN.File2TLum.Data.Curve] Error: run = ",run, " is not valid.
                 Valid values are: ", paste(as.character(run.valid), collapse=", "),
                 sep=""))
    }

    }else{

      ##check if id is valid
      id.valid <- unique(object@METADATA[,"ID"])

      if (!(id %in% id.valid)){
        stop(paste("[TLum.BIN.File2TLum.Data.Curve] Error: id = ",id, " is not a valid record id.
                   Valid values are: ",  paste(as.character(run.valid), collapse=", "),
                   sep=""))
      }
    }


  # grep id of record -------------------------------------------------------

  ##if id is set, no input for pos and run is nescessary
  if(missing(id) == TRUE){

    temp.pos <- object@METADATA$POSITION
    temp.set <- object@METADATA$SET
    temp.run <- object@METADATA$RUN

    record <- which(temp.pos == pos & temp.set == set & temp.run == run)

  }else{
    temp.id <- object@METADATA$ID

    record <- which(temp.id == id)
  }


  # Select values -----------------------------------------------------------

  new.recordType <- as.character(object@METADATA[record,"LTYPE"])
  new.curveType <- as.character(object@METADATA[record,"DTYPE"])

  new.metadata <- as.list(object@METADATA[record,])

  Tmax <- new.metadata$HIGH
  nPoints <- new.metadata$NPOINTS
  Hrate <- new.metadata$RATE
  an_time  <- new.metadata$AN_TIME
  an_temp  <- new.metadata$AN_TEMP

  temperatures.data <- calc_TL.temperature(nPoints = nPoints,
                                          Tmax = Tmax,
                                          Hrate = Hrate,
                                          an_temp = an_temp,
                                          an_time = an_time,
                                          rec_ramp2PH = rec_ramp2PH,
                                          rec_duringPH = rec_duringPH)

  new.temperatures <- get_TLum.Results(temperatures.data,"temperatures")

  new.data <- as.numeric(unlist(object@DATA[record]))
  new.error <- as.numeric(unlist(object@ERROR[record]))

  new.analysis <- list()

  new.RESERVED <- object@.RESERVED[[record]]

  # Build object ------------------------------------------------------------

  new.TLum.Data.Curve <- set_TLum.Data.Curve(recordType = new.recordType,
                                             curveType = new.curveType,
                                             temperatures = new.temperatures,
                                             data = new.data,
                                             error = new.error,
                                             metadata = new.metadata,
                                             analysis = new.analysis,
                                             .RESERVED = new.RESERVED)

  return(new.TLum.Data.Curve)
    }

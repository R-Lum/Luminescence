#' Convert Risoe.BINfileData object to an RLum.Analysis object
#'
#' Converts values from one specific position of a Risoe.BINfileData S4-class
#' object to an RLum.Analysis object.
#'
#' The [RLum.Analysis-class] object requires a set of curves for
#' specific further protocol analyses. However, the [Risoe.BINfileData-class]
#' usually contains a set of curves for different aliquots and different
#' protocol types that may be mixed up. Therefore, a conversion is needed.
#'
#' @param object [Risoe.BINfileData-class] (**required**):
#' `Risoe.BINfileData` object
#'
#' @param pos [numeric] (*optional*): position number of the `Risoe.BINfileData`
#' object for which the curves are stored in the `RLum.Analysis` object.
#' If `length(position)>1` a list of `RLum.Analysis` objects is returned.
#' If nothing is provided every position will be converted.
#' If the position is not valid `NA` is returned.
#'
#' @param grain [vector], [numeric] (*optional*):
#' grain number from the measurement to limit the converted data set
#' (e.g., `grain = c(1:48)`). Please be aware that this option may lead to
#' unwanted effects, as the output is strictly limited to the chosen grain
#' number for all position numbers
#'
#' @param run [vector], [numeric] (*optional*):
#' run number from the measurement to limit the converted data set
#' (e.g., `run = c(1:48)`).
#'
#' @param set [vector], [numeric] (*optional*):
#' set number from the measurement to limit the converted data set
#' (e.g., `set = c(1:48)`).
#'
#' @param ltype [vector], [character] (*optional*):
#' curve type to limit the converted data. Commonly allowed values are:
#' `IRSL`, `OSL`, `TL`, `RIR`, `RBR` and `USER`
#' (see also [Risoe.BINfileData-class])
#'
#' @param dtype [vector], [character] (*optional*):
#' data type to limit the converted data. Commonly allowed values are
#' listed in [Risoe.BINfileData-class]
#'
#' @param protocol [character] (*optional*):
#' sets protocol type for analysis object. Value may be used by subsequent
#' analysis functions.
#'
#' @param keep.empty [logical] (*with default*):
#' If `TRUE` (default) an `RLum.Analysis` object is returned even if it does
#' not contain any records. Set to `FALSE` to discard all empty objects.
#'
#' @param txtProgressBar [logical] (*with default*):
#' enables or disables [txtProgressBar].
#'
#' @return Returns an [RLum.Analysis-class] object.
#'
#' @note
#' The `protocol` argument of the [RLum.Analysis-class]
#' object is set to 'unknown' if not stated otherwise.
#'
#' @section Function version: 0.4.2
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [Risoe.BINfileData-class], [RLum.Analysis-class], [read_BIN2R]
#'
#' @keywords manip
#'
#' @examples
#'
#' ##load data
#' data(ExampleData.BINfileData, envir = environment())
#'
#' ##convert values for position 1
#' Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1)
#'
#' @md
#' @export
Risoe.BINfileData2RLum.Analysis<- function(
  object,
  pos = NULL,
  grain = NULL,
  run = NULL,
  set = NULL,
  ltype = NULL,
  dtype = NULL,
  protocol = "unknown",
  keep.empty = TRUE,
  txtProgressBar = FALSE
){


  # Integrity Check ---------------------------------------------------------

  if (!is(object,"Risoe.BINfileData")){
    stop("[Risoe.BINfileData2RLum.Analysis()] Input object is not of type 'Risoe.BINfileData'.", call. = FALSE)
  }

  if (!is.null(pos) && !is(pos,"numeric")){
    stop("[Risoe.BINfileData2RLum.Analysis()] Argument 'pos' has to be of type numeric.", call. = FALSE)
  }

  if (is.null(pos)) {
    pos <- unique(object@METADATA[["POSITION"]])

  } else{
    ##get and check valid positions and remove invalid numbers from the input
    positions.valid <- unique(object@METADATA[, "POSITION"])

    if (length(setdiff(pos, positions.valid)) > 0) {
      warning(
        paste0(
          "[Risoe.BINfileData2RLum.Analysis()] invalid position number skipped: ",
          paste(setdiff(pos, positions.valid), collapse = ", ")
        ),
        call. = FALSE
      )

      pos <- intersect(pos, positions.valid)

    }
  }


  # Grep run and set data ---------------------------------------------------

    ##grain
    if (is.null(grain)) {
      grain <- unique(object@METADATA[["GRAIN"]])

    }else{
      grain.valid <- unique(object@METADATA[["GRAIN"]])
      if(length(setdiff(grain, grain.valid)) > 0){
        warning(paste0("[Risoe.BINfileData2RLum.Analysis()] Invalid grain number skipped: ",
                       paste(setdiff(grain, grain.valid), collapse = ", ")), call. = FALSE)

        grain <- intersect(grain, grain.valid)

      }

    }

    ##run
    if (is.null(run)) {
      run <- unique(object@METADATA[["RUN"]])
    } else{
      if (TRUE %in% unique(unique(object@METADATA[["RUN"]]) %in% run) != TRUE) {
        ##get and check valid positions
        run.valid <-
          paste(as.character(unique(object@METADATA[, "RUN"])), collapse = ", ")

        stop(
          paste(
            "[Risoe.BINfileData2RLum.Analysis()] run = ",
            run,
            " contain invalid run(s).
            Valid runs are: ",
            run.valid,
            sep = ""
          )
        )

      }

    }

    #set
    if(is.null(set)){set <- unique(object@METADATA[["SET"]])
    } else{

      if(TRUE %in% unique(unique(object@METADATA[["SET"]]) %in% set) != TRUE){

        ##get and check valid positions
        set.valid <- paste(as.character(unique(object@METADATA[,"SET"])), collapse=", ")

        stop(paste("[Risoe.BINfileData2RLum.Analysis] set = ", set, " contain invalid set(s).
                   Valid sets are: ", set.valid, sep=""))

      }

    }

    ##ltype
    if (is.null(ltype)) {
      ltype <- unique(object@METADATA[["LTYPE"]])
    } else{
      if (TRUE %in% unique(unique(object@METADATA[, "LTYPE"]) %in% ltype) != TRUE) {
        ##get and check valid positions
        ltype.valid <-
          paste(as.character(unique(object@METADATA[, "LTYPE"])), collapse = ", ")

        stop(
          paste(
            "[Risoe.BINfileData2RLum.Analysis] ltype = ",
            ltype,
            " contain invalid ltype(s).
            Valid ltypes are: ",
            ltype.valid,
            sep = ""
          )
        )

      }

    }

    ##dtype
    if (is.null(dtype)) {
      dtype <- unique(object@METADATA[["DTYPE"]])
    } else{
      if (TRUE %in% unique(unique(object@METADATA[, "DTYPE"]) %in% dtype) != TRUE) {
        ##get and check valid positions
        dtype.valid <-
          paste(as.character(unique(object@METADATA[, "DTYPE"])), collapse = ", ")

        stop(
          paste(
            "[Risoe.BINfileData2RLum.Analysis] dtype = ",
            dtype,
            " contain invalid dtype(s).
            Valid dtypes are: ",
            dtype.valid,
            sep = ""
          )
        )

      }

    }

    # Select values and convert them-----------------------------------------------------------
    ##set progressbar to false if only one position is provided
    if(txtProgressBar & length(pos)<2){
      txtProgressBar <- FALSE

    }

    ##This loop does:
    ## (a) iterating over all possible positions
    ## (b) consider grains in all possible positions
    ## (c) consider other selections
    ## (d) create the RLum.Analysis objects

    ##set progress bar
    if(txtProgressBar){
      pb <- txtProgressBar(min=min(pos),max=max(pos), char="=", style=3)
    }

    object <- lapply(pos, function(pos){

      ##update progress bar
      if(txtProgressBar){
        setTxtProgressBar(pb, value = pos)
      }

      ##if no grain information is given, we select all grains in the particular position
      if(is.null(grain)){
        grain <- unique(object@METADATA[object@METADATA[["POSITION"]] == pos, "GRAIN"])

      }

      ##loop over the grains and produce RLum.Analysis objects
      object <- lapply(grain, function(grain){

        ##select data
        ##the NA is necessary, as FI readers like to write a NA instead of 0 in the column
        ##and this causes some trouble

        if(is.na(grain)){
          temp_id <- object@METADATA[
            object@METADATA[["POSITION"]] == pos &
              object@METADATA[["RUN"]] %in% run &
              object@METADATA[["SET"]] %in% set &
              object@METADATA[["LTYPE"]] %in% ltype &
              object@METADATA[["DTYPE"]] %in% dtype
            , "ID"]


        }else{
          temp_id <- object@METADATA[
            object@METADATA[["POSITION"]] == pos &
              object@METADATA[["GRAIN"]] == grain &
              object@METADATA[["RUN"]] %in% run &
              object@METADATA[["SET"]] %in% set &
              object@METADATA[["LTYPE"]] %in% ltype &
              object@METADATA[["DTYPE"]] %in% dtype
            , "ID"]


        }

        ##create curve object
        object <- set_RLum(
          class = "RLum.Analysis",
          records = lapply(temp_id,function(x) {
            .Risoe.BINfileData2RLum.Data.Curve(object, id = x)
          }),
          protocol = protocol,
          originator = "Risoe.BINfileData2RLum.Analysis"
        )

        if (!keep.empty && length(object@records) == 0)
          return(NULL)

        ##add unique id of RLum.Analysis object to each curve object as .pid using internal function
        .set_pid(object)

        return(object)

      })

      return(object)

    })

    ##this is necessary to not break with previous code, i.e. if only one element is included
    ##the output is RLum.Analysis and not a list of it
    if(length(object) == 1){

      # special case: single grain data with only 1 position produces a nested list
      # the outer one is of length 1, the nested list has length 100 (100 grains)
      if (is.list(object[[1]]) && length(object[[1]]) > 1)
        invisible(unlist(object))
      else
        invisible(object[[1]][[1]])

    }else{

      invisible(unlist(object))

    }

}

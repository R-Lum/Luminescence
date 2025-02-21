#' @title Convert Risoe.BINfileData object to an RLum.Analysis object
#'
#' @description
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
#' If the position is not valid `NULL` is returned.
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
#' enable/disable the progress bar.
#'
#' @return Returns an [RLum.Analysis-class] object.
#'
#' @note
#' The `protocol` argument of the [RLum.Analysis-class]
#' object is set to 'unknown' if not stated otherwise.
#'
#' @section Function version: 0.4.3
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
) {
  .set_function_name("Risoe.BINfileData2RLum.Analysis")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity tests --------------------------------------------------------

  .validate_class(object, "Risoe.BINfileData")
  if (!is.null(pos)) {
    .validate_class(pos, c("numeric", "integer"))
  }

  if (is.null(pos)) {
    pos <- unique(object@METADATA[["POSITION"]])

  } else{
    ##get and check valid positions and remove invalid numbers from the input
    positions.valid <- unique(object@METADATA[, "POSITION"])

    if (length(setdiff(pos, positions.valid)) > 0) {
      .throw_warning("Invalid position number skipped: ",
                     .collapse(setdiff(pos, positions.valid), quote = FALSE))
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
        .throw_warning("Invalid grain number skipped: ",
                       .collapse(setdiff(grain, grain.valid), quote = FALSE))

        grain <- intersect(grain, grain.valid)
      }
    }

    ##run
    if (is.null(run)) {
      run <- unique(object@METADATA[["RUN"]])
    } else{
      if (TRUE %in% unique(unique(object@METADATA[["RUN"]]) %in% run) != TRUE) {
        ##get and check valid positions
        .throw_error("run = ", .collapse(run, quote = FALSE),
                     " contains invalid runs. Valid runs are: ",
                     .collapse(unique(object@METADATA[, "RUN"]),
                               quote = FALSE))
      }
    }

    #set
    if(is.null(set)){set <- unique(object@METADATA[["SET"]])
    } else{

      if(TRUE %in% unique(unique(object@METADATA[["SET"]]) %in% set) != TRUE){

        ##get and check valid positions
        .throw_error("set = ", .collapse(set, quote = FALSE),
                     " contains invalid sets. Valid sets are: ",
                     .collapse(unique(object@METADATA[, "SET"]),
                               quote = FALSE))
      }
    }

    ##ltype
    if (is.null(ltype)) {
      ltype <- unique(object@METADATA[["LTYPE"]])
    } else{
      if (TRUE %in% unique(unique(object@METADATA[, "LTYPE"]) %in% ltype) != TRUE) {
        ##get and check valid positions
        .throw_error("ltype = ", .collapse(ltype),
                     " contains invalid ltypes. Valid ltypes are: ",
                     .collapse(unique(object@METADATA[, "LTYPE"])))
      }
    }

    ##dtype
    if (is.null(dtype)) {
      dtype <- unique(object@METADATA[["DTYPE"]])
    } else{
      if (TRUE %in% unique(unique(object@METADATA[, "DTYPE"]) %in% dtype) != TRUE) {
        ##get and check valid positions
        .throw_error("dtype = ", .collapse(dtype),
                     " contains invalid dtypes. Valid dtypes are: ",
                     .collapse(unique(object@METADATA[, "DTYPE"])))
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

      ##loop over the grains and produce RLum.Analysis objects
      object <- lapply(grain, function(grain){

        ## select data
        ## the NA check for grain is necessary as FI readers like to report
        ## NA instead of 0 in that column, and this causes some trouble
        temp_id <- object@METADATA[
              object@METADATA[["POSITION"]] == pos &
              (is.na(object@METADATA[["GRAIN"]]) |
               object@METADATA[["GRAIN"]] == grain) &
              object@METADATA[["RUN"]] %in% run &
              object@METADATA[["SET"]] %in% set &
              object@METADATA[["LTYPE"]] %in% ltype &
              object@METADATA[["DTYPE"]] %in% dtype
            , "ID"]

        ## if the input object is empty, bypass the creation of curve objects
        if (length(object@DATA) == 0) {
          message("Empty Risoe.BINfileData object detected")
          records <- list()
        } else {
          ## create curve object
          records <- lapply(temp_id, function(x) {
            ## skip ROI information
            if (!is.null(object@METADATA[["RECTYPE"]]) &&
                object@METADATA[["RECTYPE"]][x] == 128)
              set_RLum(class = "RLum.Data.Curve")
            else
              .Risoe.BINfileData2RLum.Data.Curve(object, id = x)
          })
        }

        ## create the RLum.Analysis object
        object <- set_RLum(
          class = "RLum.Analysis",
          records = records,
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

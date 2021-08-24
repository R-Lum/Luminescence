#' Convert an element from a Risoe.BINfileData object to an RLum.Data.Curve
#' object
#'
#' The function converts one specified single record from a Risoe.BINfileData
#' object to an RLum.Data.Curve object.
#'
#' The function extracts all `METADATA` from the `Risoe.BINfileData`
#' object and stores them in the `RLum.Data.Curve` object. This function
#' can be used stand-alone, but is the base function for [Risoe.BINfileData2RLum.Analysis].
#'
#' @param object [Risoe.BINfileData-class] (**required**):
#' `Risoe.BINfileData` object
#'
#' @param id [integer] (**required**):
#' record id in the `Risoe.BINfileData` object of the curve that is to be
#' stored in the `RLum.Data.Curve` object. If no value for id is provided,
#' the record has to be specified by `pos`, `set` and `run`.
#'
#' @param pos [integer] (*optional*):
#' record position number in the `Risoe.BINfileData` object of the curve that
#' is to be stored in the `RLum.Data.Curve` object. If a value for `id` is
#' provided, this argument is ignored.
#'
#' @param run [integer] (*optional*):
#' record run number in the `Risoe.BINfileData` object of the curve that is
#' to be stored in the `RLum.Data.Curve` object. If a value for `id` is
#' provided, this argument is ignored.
#'
#' @param set [integer] (*optional*):
#' record set number in the `Risoe.BINfileData` object of the curve that is
#' to be stored in the `RLum.Data.Curve` object. If a value for `id` is
#' provided, this argument is ignored.
#'
#' @return Returns an [RLum.Data.Curve-class] object.
#'
#' @note
#' Due to changes in the BIN-file (version 3 to version 4) format the recalculation of TL-curves might be not
#' overall correct for cases where the TL measurement is combined with a preheat.
#'
#' @section Function version: 0.5.0
#'
#' @author
#' Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)\cr
#' Christoph Burow, Universtiy of Cologne (Germany)
#'
#' @seealso [Risoe.BINfileData2RLum.Analysis], [set_RLum],
#' [RLum.Data.Curve-class], [RLum.Analysis-class], [Risoe.BINfileData-class],
#' [plot_RLum]
#'
#' @keywords manip
#'
#' @examples
#'
#' ##get package example data
#' data(ExampleData.BINfileData, envir = environment())
#'
#' ##convert one record
#' Risoe.BINfileData2RLum.Data.Curve(CWOSL.SAR.Data, id = 1)
#'
#' @md
#' @noRd
.Risoe.BINfileData2RLum.Data.Curve <- function(
  object,
  id,
  pos,
  run,
  set
){

  ##disaggregate object ... this makes it much faster below
  ##we could also access via index, not number, but this is far to risky, as
  ##every update in the BIN-file version will break the code here
  METADATA <- as.list(object@METADATA)
  DATA <- object@DATA

  # grep id of record -------------------------------------------------------
  ##if id is set, no input for pos and rund is nescessary
  if (missing(id)) {
    id <- METADATA[METADATA[["POSITION"]] == pos &
                     METADATA[["SET"]] == set &
                     METADATA[["RUN"]] == run,
                   "ID"]

  }


  ##grep info elements
  info <- lapply(1:length(names(METADATA)), function(x){METADATA[[x]][id]})
  names(info) <- names(METADATA)

  # Build object ------------------------------------------------------------
  set_RLum(
    class = "RLum.Data.Curve",
    recordType = METADATA[["LTYPE"]][id],
    data =  src_create_RLumDataCurve_matrix(
      DATA = DATA[[id]],
      NPOINTS = METADATA[["NPOINTS"]][id],
      VERSION = METADATA[["VERSION"]][id],
      LTYPE = METADATA[["LTYPE"]][id],
      LOW =  METADATA[["LOW"]][id],
      HIGH =  METADATA[["HIGH"]][id],
      AN_TEMP = METADATA[["AN_TEMP"]][id],
      TOLDELAY = METADATA[["TOLDELAY"]][id],
      TOLON = METADATA[["TOLON"]][id],
      TOLOFF = METADATA[["TOLOFF"]][id]

    ),
    info = info
  )

}

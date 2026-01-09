#' @title Convert an element from a Risoe.BINfileData object to an RLum.Data.Curve
#' object
#'
#' @description
#' The function converts one specified single record from a Risoe.BINfileData
#' object to an RLum.Data.Curve object.
#'
#' @details
#' The function extracts all `METADATA` from the `Risoe.BINfileData`
#' object and stores them in the [Luminescence::RLum.Data.Curve-class] object. This function
#' can be used stand-alone, but is the base function for [Luminescence::Risoe.BINfileData2RLum.Analysis].
#'
#' @param object [Luminescence::Risoe.BINfileData-class] (**required**):
#' `Risoe.BINfileData` object
#'
#' @param id [integer] (**required**):
#' record id in the `Risoe.BINfileData` object of the curve that is to be
#' stored in the [Luminescence::RLum.Data.Curve-class] object. If no value for id is provided,
#' the record has to be specified by `pos`, `set` and `run`.
#'
#' @param pos [integer] (*optional*):
#' record position number in the `Risoe.BINfileData` object of the curve that
#' is to be stored in the [Luminescence::RLum.Data.Curve-class] object. If a value for `id` is
#' provided, this argument is ignored.
#'
#' @param run [integer] (*optional*):
#' record run number in the `Risoe.BINfileData` object of the curve that is
#' to be stored in the [Luminescence::RLum.Data.Curve-class] object. If a value for `id` is
#' provided, this argument is ignored.
#'
#' @param set [integer] (*optional*):
#' record set number in the `Risoe.BINfileData` object of the curve that is
#' to be stored in the [Luminescence::RLum.Data.Curve-class] object. If a value for `id` is
#' provided, this argument is ignored.
#'
#' @return Returns an [Luminescence::RLum.Data.Curve-class] object.
#'
#' @note
#' Due to changes in the BIN-file (version 3 to version 4) format the recalculation of TL-curves might be not
#' overall correct for cases where the TL measurement is combined with a preheat.
#'
#' @section Function version: 0.5.0
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#' Christoph Burow, Universtiy of Cologne (Germany)
#'
#' @seealso [Luminescence::Risoe.BINfileData2RLum.Analysis], [Luminescence::set_RLum],
#' [Luminescence::RLum.Data.Curve-class], [Luminescence::RLum.Analysis-class],
#' [Luminescence::Risoe.BINfileData-class],[Luminescence::plot_RLum]
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
  ##if id is set, no input for pos and run is necessary
  if (missing(id)) {
    id <- METADATA$ID[METADATA[["POSITION"]] == pos &
                      METADATA[["SET"]] == set &
                      METADATA[["RUN"]] == run]
  }

  ##grep info elements
  info <- lapply(METADATA, function(x) x[id])
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
